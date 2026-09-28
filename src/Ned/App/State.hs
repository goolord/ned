-- | Everything the application is, between frames.
--
-- The editor, the file tree and the finder keep their own state; this is what
-- is left over and belongs to none of them: which files are open and in what
-- format, which of them is in front, which menu is down, which bar is up,
-- what has the keyboard, and the change that is waiting to be agreed to. A
-- frame reads one of these and writes the next.
module Ned.App.State
  ( App (..)
  , Doc (..)
  , Bar (..)
  , Pending (..)
  , Placement (..)
  , newApp
  , openPath

    -- * The tabs
  , appDocs
  , manyTabs
  , tabWith
  , findTab
  , activeDoc
  , selectDoc
  , stepDoc
  , newDoc
  , blankDoc
  , closeDoc
  , everyEditor
  , docName
  , docDirty

    -- * What follows from the state
  , modalUp
  , findMarks
  , otherWords
  , titleFor
  ) where

import Data.IORef (IORef, newIORef)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Backend.Sdl (FileDialogId)
import qualified Ned.Buffer as B
import Ned.Complete (Source, buffersSource)
import Ned.Complete.Tags (Tags, tagSource)
import Ned.Config (Config (..))
import Ned.Editor
import Ned.Editor.Vim (newVim)
import Ned.File
import Ned.FileTree (FileTree)
import qualified Ned.FileTree as FT
import Ned.Highlight (languageFor, plainText)
import Ned.Lsp (Servers, newServers)
import Ned.Picker (Picker)
import System.Directory (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath (equalFilePath, takeFileName)

--------------------------------------------------------------------------------
-- The state
--------------------------------------------------------------------------------

-- | The bar under the editor.
data Bar = BarNone | BarFind | BarGoto
  deriving (Eq)

-- | Something that would throw text away, held until that is agreed to.
data Pending
  = -- | Close the tab of this key.
    PendingClose Int
  | -- | Open a file, by its absolute path, in the tab in front, in place of
    -- what it holds.
    PendingReplace FilePath
  | -- | An untitled file in the tab in front, in place of what it holds.
    PendingNew
  | PendingQuit

-- | Where a file that is not open yet opens.
data Placement
  = -- | In a tab of its own, after the one in front.
    InNewTab
  | -- | In the tab in front, in place of what it holds.
    InFrontTab
  deriving (Eq)

-- | A file open in a tab: its text, where it is on disk, and what of it is
-- not its text. The key names its tab, and is never given to another.
data Doc = Doc
  { docKey :: !Int
  , docEditor :: !Editor
  , docPath :: !(Maybe FilePath)
  , docFormat :: !FileFormat
  }

-- | The tabs are a zipper whose focus is spread over the record: the file in
-- front is 'appEditor', 'appPath' and 'appFormat' under 'appDocKey', and the
-- others are either side of it. Everything that works on the file in front
-- reads and writes those three and never has to find it among the others.
data App = App
  { appEditor :: !Editor
  , appPath :: !(Maybe FilePath)
  , appFormat :: !FileFormat
  , appDocKey :: !Int
  , appBefore :: ![Doc]
  -- ^ The tabs before the one in front, the nearest first.
  , appAfter :: ![Doc]
  -- ^ The tabs after the one in front, in order.
  , appNextKey :: !Int
  , appStatus :: !Text
  , appOpenMenu :: !Text
  , appMenuSwallow :: !Text
  -- ^ The menu whose own button's press just closed it, until the release.
  , appOpenDlg :: !(Maybe FileDialogId)
  , appSaveDlg :: !(Maybe (FileDialogId, Int))
  -- ^ The save dialog, and the key of the tab it is up for.
  , appBar :: !Bar
  , appBarFocus :: !Bool
  -- ^ Whether the bar's field has the keyboard, and not the editor.
  , appFindText :: !Text
  , appGotoText :: !Text
  , appPending :: !(Maybe Pending)
  , appTitle :: !Text
  -- ^ The window's title as last set.
  , appTree :: !FileTree
  , appTreeShown :: !Bool
  , appTreeFocus :: !Bool
  -- ^ Whether the tree has the keyboard, and not the editor.
  , appPicker :: !(Maybe Picker)
  -- ^ The fuzzy finder, while it is up. It is a modal over the window, so
  -- while it is there nothing else reads a key.
  , appConfig :: !Config
  -- ^ The settings, as last read.
  , appConfigPath :: !(Maybe FilePath)
  -- ^ The file they were read from, which is watched for changes.
  , appConfigSeen :: !Int
  -- ^ How many times the file has been read again since the window opened.
  , appServers :: !Servers
  -- ^ The language servers started so far.
  , appAnswers :: !(IORef [App -> IO App])
  -- ^ What the language servers' answers do to the application, oldest
  -- first, put here by the threads that waited for them for the next frame
  -- to do.
  }

-- | A fresh application on some settings, with its tree on the directory the
-- program was started in.
newApp :: Config -> IO App
newApp cfg = do
  cwd <- getCurrentDirectory
  servers <- newServers
  answers <- newIORef []
  pure
    App
      { -- The first tab is set up as the settings say, and every tab after it
        -- takes after the one it opens beside.
        appEditor =
          (newEditor plainText B.empty)
            { edFontSize = cfgBufferFontSize cfg
            , edShowWhitespace = cfgShowIndentation cfg
            , edVim = if cfgVimKeys cfg then Just newVim else Nothing
            }
      , appPath = Nothing
      , appFormat = FileFormat LF False
      , appDocKey = 0
      , appBefore = []
      , appAfter = []
      , appNextKey = 1
      , appStatus = "Ready"
      , appOpenMenu = ""
      , appMenuSwallow = ""
      , appOpenDlg = Nothing
      , appSaveDlg = Nothing
      , appBar = BarNone
      , appBarFocus = False
      , appFindText = ""
      , appGotoText = ""
      , appPending = Nothing
      , appTitle = ""
      , appTree = FT.newFileTree cwd
      , appTreeShown = cfgShowFileTree cfg
      , appTreeFocus = False
      , appPicker = Nothing
      , appConfig = cfg
      , appConfigPath = Nothing
      , appConfigSeen = 0
      , appServers = servers
      , appAnswers = answers
      }

--------------------------------------------------------------------------------
-- Opening a file
--------------------------------------------------------------------------------

-- | Open a file, with the caret on a line of it if one is given, or start a
-- new one under a name that does not exist yet. A file that is open already
-- has its tab brought to the front instead, and a file that cannot be read
-- leaves every tab as it was.
--
-- A new tab goes after the one in front, unless that one is an untitled file
-- with nothing in it, which is put away for it. Opened in the tab in front,
-- the file takes the place of what that tab held, whatever it was: asking
-- about its changes is for the caller.
openPath :: Placement -> Maybe Int -> FilePath -> App -> IO App
openPath placement line path0 app = do
  path <- makeAbsolute path0
  exists <- doesFileExist path
  let goto = maybe id (B.gotoLine . (+ 1)) line
      -- The tree follows the file: it opens the folders down to it, and
      -- moves to the folder the file is in when it is somewhere else.
      revealed a = a {appTree = FT.reveal path (appTree a)}
      fresh buf format msg =
        revealed $
          insertDoc
            (placement == InFrontTab || blank app)
            (newEditor (languageFor path) (goto buf))
            (Just path)
            format
            app {appStatus = msg}
  case findTab path app of
    Just doc ->
      let shown = selectDoc (docKey doc) app
       in pure . revealed $
            if isJust line
              then shown {appEditor = revealCaret (appEditor shown) {edBuffer = goto (edBuffer (appEditor shown))}}
              else shown
    Nothing
      | not exists -> pure (fresh B.empty (FileFormat LF False) ("New file " <> T.pack (takeFileName path)))
      | otherwise ->
          loadFile path >>= \case
            Left err -> pure app {appStatus = "Could not open " <> T.pack path <> ": " <> err}
            Right loaded ->
              pure . fresh (loadedBuffer loaded) (loadedFormat loaded) $
                if loadedLossy loaded
                  then "Opened " <> T.pack (takeFileName path) <> ", which is not UTF-8: its other bytes are shown as " <> T.singleton (toEnum 0xFFFD) <> " and saving will not bring them back"
                  else "Opened " <> T.pack (takeFileName path)
  where
    -- An untitled file with nothing in it and nothing to undo.
    blank a = isNothing (appPath a) && B.size buf == 0 && not (B.canUndo buf)
      where
        buf = edBuffer (appEditor a)

--------------------------------------------------------------------------------
-- The tabs
--------------------------------------------------------------------------------

-- | The tab holding a file, if one does.
tabWith :: FilePath -> App -> IO (Maybe Doc)
tabWith path app = (`findTab` app) <$> makeAbsolute path

-- | The tab holding a file, by its absolute path.
findTab :: FilePath -> App -> Maybe Doc
findTab path = find (maybe False (equalFilePath path) . docPath) . appDocs

-- | Whether there is more than the one tab.
manyTabs :: App -> Bool
manyTabs app = not (null (appBefore app) && null (appAfter app))

-- | Every tab, in the order the strip shows them.
appDocs :: App -> [Doc]
appDocs app = reverse (appBefore app) <> [activeDoc app] <> appAfter app

-- | The tab in front, gathered up from the record.
activeDoc :: App -> Doc
activeDoc app = Doc (appDocKey app) (appEditor app) (appPath app) (appFormat app)

-- | Put a tab in front, in the place of the one that was, which goes nowhere:
-- the caller has put it somewhere already, or means to lose it.
showDoc :: Doc -> App -> App
showDoc doc app = app {appEditor = docEditor doc, appPath = docPath doc, appFormat = docFormat doc, appDocKey = docKey doc}

-- | The tab in front put behind, before where the next one will go.
pushActive :: App -> App
pushActive app = app {appBefore = activeDoc app : appBefore app}

-- | Bring the tab of this key to the front. A key no tab has changes nothing.
selectDoc :: Int -> App -> App
selectDoc key app =
  case break ((== key) . docKey) (appDocs app) of
    (before, doc : after) | key /= appDocKey app -> showDoc doc app {appBefore = reverse before, appAfter = after}
    _ -> app

-- | Bring the next tab to the front, or the one before, round from the ends.
stepDoc :: Bool -> App -> App
stepDoc forward app
  | forward, next : _ <- appAfter app = selectDoc (docKey next) app
  | forward, first : _ <- docs = selectDoc (docKey first) app
  | not forward, prev : _ <- appBefore app = selectDoc (docKey prev) app
  | otherwise = selectDoc (docKey (last docs)) app
  where
    docs = appDocs app

-- | A new tab, untitled and empty, after the one in front and in front now.
newDoc :: App -> App
newDoc = insertDoc False (newEditor plainText B.empty) Nothing (FileFormat LF False)

-- | An untitled, empty file in place of the tab in front, whatever it held.
blankDoc :: App -> App
blankDoc = insertDoc True (newEditor plainText B.empty) Nothing (FileFormat LF False)

-- | A tab under a key of its own and in front, shown as the one that was in
-- front is: after it, or in its place when that is to go.
insertDoc :: Bool -> Editor -> Maybe FilePath -> FileFormat -> App -> App
insertDoc replace ed path format app =
  showDoc
    Doc
      { docKey = appNextKey app
      , docEditor = ed {edFontSize = edFontSize front, edShowWhitespace = edShowWhitespace front, edVim = newVim <$ edVim front}
      , docPath = path
      , docFormat = format
      }
    (if replace then app else pushActive app) {appNextKey = appNextKey app + 1}
  where
    front = appEditor app

-- | Close the tab of this key, whatever it holds. Closing the tab in front
-- brings the one before it forward, or the one after it when it was first;
-- closing the last tab leaves an untitled one, since there is always a file
-- to type into.
closeDoc :: Int -> App -> App
closeDoc key app
  | key /= appDocKey app = app {appBefore = drop' (appBefore app), appAfter = drop' (appAfter app)}
  | prev : rest <- appBefore app = showDoc prev app {appBefore = rest}
  | next : rest <- appAfter app = showDoc next app {appAfter = rest}
  | otherwise = (newDoc app) {appBefore = []}
  where
    drop' = filter ((/= key) . docKey)

-- | Change how the text of every tab is shown. How big it is and whether its
-- indentation is marked are the reader's, not the file's, so they are the
-- same in every tab.
everyEditor :: (Editor -> Editor) -> App -> App
everyEditor f app =
  app
    { appEditor = f (appEditor app)
    , appBefore = map onDoc (appBefore app)
    , appAfter = map onDoc (appAfter app)
    }
  where
    onDoc d = d {docEditor = f (docEditor d)}

-- | What a tab is called: its file's name, or "Untitled".
docName :: Doc -> Text
docName = maybe "Untitled" (T.pack . takeFileName) . docPath

-- | Whether a tab has changes to save.
docDirty :: Doc -> Bool
docDirty = B.isDirty . edBuffer . docEditor

--------------------------------------------------------------------------------
-- What follows from the state
--------------------------------------------------------------------------------

-- | Whether a modal is up: the question about unsaved changes, or the finder.
-- While one is, it is the only thing that reads a key.
modalUp :: App -> Bool
modalUp app = isJust (appPending app) || isJust (appPicker app)

-- | What the editor marks the matches of: what the find bar holds, while it
-- is up.
findMarks :: App -> Text
findMarks app = if appBar app == BarFind then appFindText app else ""

-- | Where words to complete the file in front come from, besides the file
-- itself: the other tabs, in the order the strip shows them, and then the
-- names in the tags file.
otherWords :: Tags -> App -> Source
otherWords tags app =
  buffersSource
    [ (docName d, edLang (docEditor d), edBuffer (docEditor d))
    | d <- appDocs app
    , docKey d /= appDocKey app
    ]
    <> tagSource tags

-- | The name of the file in front, starred while it has changes to save.
titleFor :: App -> Text
titleFor app =
  docName (activeDoc app)
    <> (if docDirty (activeDoc app) then " *" else "")
    <> " - ned"
