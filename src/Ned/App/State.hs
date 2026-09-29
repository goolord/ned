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
  , PaletteState (..)
  , Placement (..)
  , Tip (..)
  , WindowLayout (..)
  , Pane (..)
  , HeldTab (..)
  , newApp
  , openPath

    -- * The panes of the row
  , treePaneId
  , editorPaneId
  , paneOf
  , appAllPanes
  , focusPane
  , onPane
  , paneDocs

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
  , hasSeparateMarkdownPreview
  , mergeMarkdownPreview
  , everyEditor
  , docName
  , docDirty

    -- * A tab on its way
  , holdTab
  , rewindDrag
  , noteDrop
  , landHeldTab

    -- * What follows from the state
  , modalUp
  , findMarks
  , otherWords
  , titleFor
  , frontDiagnostics
  , hoverAt
  , diagnosticSpans
  , lspOffset
  ) where

import Data.IORef (IORef, newIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import NanoUI (DragPhase (..), GridNode, Rect (..), Size (..), V2 (..), rectContains)
import NanoUI.Backend.Sdl (FileDialogId)
import NanoUI.Markdown (MarkdownDoc)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ned.Lsp (Diagnostic (..), Latest, Servers, fromUtf16, newLatest, newServers)
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

data PaletteState = PaletteState
  { paletteQuery :: !Text
  , paletteCursor :: !Int
  }
  deriving (Eq)

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
  , docMarkdownPreviewOf :: !(Maybe Int)
  -- ^ The source tab rendered by this read-only preview tab.
  }

-- | A pane of the row the tree and the editors share: the tabs of its own,
-- and the one of them it shows. The pane the application is working in has
-- its shown tab spread over the 'App' record instead ('appEditor',
-- 'appPath', 'appFormat', 'appDocKey', 'appBefore', 'appAfter'); the record
-- here is for every pane beside it. Nothing but 'paneKey' is read of a pane
-- without its tabs.
data Pane = Pane
  { paneKey :: !Word64
  -- ^ The pane's id in the editors' pane grid.
  , paneFront :: !Doc
  -- ^ The tab the pane shows.
  , paneBefore :: ![Doc]
  -- ^ The tabs before the one shown, the nearest first.
  , paneAfter :: ![Doc]
  -- ^ The tabs after the one shown, in order.
  }

-- | A tab the pointer is holding, on its way from the strip it was taken
-- from to wherever it will land. The strip's own drag ('useDrag') holds it
-- from the press carried past the threshold to the frame the button comes
-- up; where it would land, the strips and panes say as the pointer goes: at
-- a place in a strip, into another pane over its body, or -- when nothing
-- has said -- into a pane of its own, which the editors' pane grid proposes
-- and the release commits.
data HeldTab = HeldTab
  { htDoc :: !Int
  -- ^ The tab's key.
  , htFrom :: !Word64
  -- ^ The pane it was taken from.
  , htPos :: !V2
  -- ^ Where the pointer is.
  , htPhase :: !DragPhase
  -- ^ What the drag is doing this frame; the release is the landing.
  , htDrop :: !(Maybe (Word64, Int))
  -- ^ Where a release would put it: into this pane's strip, before the tab
  -- now at this index of it.
  }
  deriving (Eq)

-- | The tabs of the pane in front are a zipper whose focus is spread over
-- the record: the file in front is 'appEditor', 'appPath' and 'appFormat'
-- under 'appDocKey', and the others are either side of it. Everything that
-- works on the file in front reads and writes those three and never has to
-- find it among the others. The panes beside the one in front keep their own
-- zippers, in 'appPanes', whole.
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
  , appPaneKey :: !Word64
  -- ^ The pane in front, whose shown tab is the one spread over this record.
  , appPanes :: ![Pane]
  -- ^ Every pane of the editors' grid beside the one in front.
  , appClosePane :: !(Maybe Word64)
  -- ^ A pane the tabs have emptied, for its own pane view to close in the
  -- pane grid ('pgcClose') and clear.
  , appTabDrag :: !(Maybe HeldTab)
  -- ^ The tab the pointer is holding, while it holds one.
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
  , appFindMatching :: !B.Matching
  -- ^ How the find bar's text is matched: in its case, and as a whole word.
  , appGotoText :: !Text
  , appMarkdownPreview :: !Bool
  -- ^ Whether the active document's Markdown preview tab is open.
  , appRequestMarkdownPreview :: !(Maybe Int)
  -- ^ The source tab whose preview pane the view should split off.
  , appMarkdownPreviewOf :: !(Maybe Int)
  -- ^ The Markdown source when the active tab is its preview.
  , appFocusMarkdownLinks :: !(Maybe Int)
  -- ^ Whether the editor yields keyboard focus to the Markdown link controls.
  , appRequestMarkdownLinkFocus :: !(Maybe Int)
  -- ^ A one-frame request to focus the preview's first link-copy button.
  , appMarkdownCache :: !(Map Int (Int, Text, MarkdownDoc))
  -- ^ Parsed previews, keyed by source tab and buffer version.
  , appCommandPalette :: !(Maybe PaletteState)
  -- ^ The command palette, while it is open.
  , appPending :: !(Maybe Pending)
  , appTitle :: !Text
  -- ^ The window's title as last set.
  , appTree :: !FileTree
  , appTreeShown :: !Bool
  , appLayout :: !WindowLayout
  -- ^ The window's size and pane arrangement, as last drawn: what is saved
  -- when the window closes.
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
  , appSync :: !Latest
  -- ^ The thread that tells the servers what the files hold as they change.
  , appSynced :: !(FilePath, Int)
  -- ^ The file in front and its version, as last handed to that thread.
  , appDiagnostics :: !(Map FilePath [Diagnostic])
  -- ^ What the servers last said is wrong in each file.
  , appHover :: !(Maybe ((Int, Int, Int), Tip))
  -- ^ What is shown by the caret, for as long as the caret and the text are
  -- where they were when it was put up ('hoverAt').
  }

-- | How the window was left: its size, whether it filled the screen, how wide
-- the tree was, and the tree/editor pane arrangement. Whether the tree was
-- shown is 'appTreeShown'.
data WindowLayout = WindowLayout
  { layoutSize :: !Size
  -- ^ In layout units, as the window opens at. A window that fills the
  -- screen keeps the size it had before, which is the one it goes back to.
  , layoutMaximized :: !Bool
  , layoutTreeWidth :: !Float
  -- ^ As it was last drawn, which a tree put away keeps.
  , layoutOuterGrid :: !(Maybe GridNode)
  -- ^ The tree and editor panes, as arranged when the window was last closed.
  }
  deriving (Eq, Show)

-- | What is shown by the caret: what a server said of what is under it, or
-- what it found wrong where a jump put the caret.
data Tip
  = TipDoc MarkdownDoc
  | TipDiagnostics [Diagnostic]

-- | A fresh application on some settings, with its tree on the directory the
-- program was started in.
newApp :: Config -> IO App
newApp cfg = do
  cwd <- getCurrentDirectory
  servers <- newServers
  answers <- newIORef []
  sync <- newLatest
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
      , appPaneKey = editorPaneId
      , appPanes = []
      , appClosePane = Nothing
      , appTabDrag = Nothing
      , appStatus = "Ready"
      , appOpenMenu = ""
      , appMenuSwallow = ""
      , appOpenDlg = Nothing
      , appSaveDlg = Nothing
      , appBar = BarNone
      , appBarFocus = False
      , appFindText = ""
      , appFindMatching = B.Matching False False
      , appGotoText = ""
      , appMarkdownPreview = False
      , appRequestMarkdownPreview = Nothing
      , appMarkdownPreviewOf = Nothing
      , appFocusMarkdownLinks = Nothing
      , appRequestMarkdownLinkFocus = Nothing
      , appMarkdownCache = Map.empty
      , appCommandPalette = Nothing
      , appPending = Nothing
      , appTitle = ""
      , appTree = FT.newFileTree cwd
      , appTreeShown = cfgShowFileTree cfg
      , appLayout =
          WindowLayout
            { layoutSize = Size (fromIntegral (cfgWindowWidth cfg)) (fromIntegral (cfgWindowHeight cfg))
            , layoutMaximized = False
            , layoutTreeWidth = FT.defaultTreeWidth
            , layoutOuterGrid = Nothing
            }
      , appTreeFocus = False
      , appPicker = Nothing
      , appConfig = cfg
      , appConfigPath = Nothing
      , appConfigSeen = 0
      , appServers = servers
      , appAnswers = answers
      , appSync = sync
      , appSynced = ("", -1)
      , appDiagnostics = Map.empty
      , appHover = Nothing
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
-- The panes of the row
--------------------------------------------------------------------------------

-- The row the tree and the editors share is a pane grid inside a pane grid:
-- the tree is one pane of the outer grid and the editors' grid is the other,
-- so putting the tree away is the outer grid's maximizing, and each strip of
-- tabs is a pane of the inner grid, which makes a pane of its own for a tab
-- dropped off the strips. A pane the tabs have emptied is the grid's to
-- close: 'appClosePane' names it, and its own pane closes it as it is drawn.

-- | The tree pane's id, and the pane beside it that holds the editors'
-- grid, which are the ids the outer row starts from. The editors' panes
-- have ids of the inner grid's own.
treePaneId, editorPaneId :: Word64
treePaneId = 1
editorPaneId = 2

-- | The pane in front, gathered up from the record.
focusedPane :: App -> Pane
focusedPane a = Pane (appPaneKey a) (activeDoc a) (appBefore a) (appAfter a)

-- | Every pane, the one in front first.
appAllPanes :: App -> [Pane]
appAllPanes a = focusedPane a : appPanes a

-- | The pane of this id, the one in front included.
paneOf :: Word64 -> App -> Maybe Pane
paneOf k a
  | k == appPaneKey a = Just (focusedPane a)
  | otherwise = find ((== k) . paneKey) (appPanes a)

-- | Put a pane's tabs back, wherever the pane is: into the record when it is
-- beside the one in front, into this record when it is the one in front.
putPane :: Pane -> App -> App
putPane p a
  | paneKey p == appPaneKey a =
      (showDoc (paneFront p) a) {appBefore = paneBefore p, appAfter = paneAfter p}
  | otherwise = a {appPanes = [if paneKey q == paneKey p then p else q | q <- appPanes a]}

-- | Do what this says to one pane's tabs.
onPane :: Word64 -> (Pane -> Pane) -> App -> App
onPane k f a = maybe a (\p -> putPane (f p) a) (paneOf k a)

-- | Make the pane of this id the one in front. The pane that was, with the
-- tab of its own it was showing, takes a place among the others; the tabs of
-- the pane that comes forward are this record's from now on.
focusPane :: Word64 -> App -> App
focusPane k a
  | k == appPaneKey a = a
  | otherwise = case break ((== k) . paneKey) (appPanes a) of
      (_, p : _) -> setFocusedPane p a {appPanes = [if paneKey q == k then focusedPane a else q | q <- appPanes a]}
      _ -> a

setFocusedPane :: Pane -> App -> App
setFocusedPane p a =
  (showDoc (paneFront p) a)
    { appPaneKey = paneKey p
    , appBefore = paneBefore p
    , appAfter = paneAfter p
    }

-- | The pane holding this tab, whichever pane that is.
paneOfDoc :: Int -> App -> Maybe Pane
paneOfDoc key = find (any ((== key) . docKey) . paneDocs) . appAllPanes

-- | A pane's tabs, in the order its strip shows them.
paneDocs :: Pane -> [Doc]
paneDocs p = reverse (paneBefore p) <> [paneFront p] <> paneAfter p

-- | The tab of this key of a pane's.
paneDoc :: Int -> Pane -> Maybe Doc
paneDoc key = find ((== key) . docKey) . paneDocs

-- | Bring a tab of this pane to its front.
paneSelect :: Int -> Pane -> Pane
paneSelect key p = case break ((== key) . docKey) (paneDocs p) of
  (before, d : after) -> p {paneFront = d, paneBefore = reverse before, paneAfter = after}
  _ -> p

-- | Take a tab out of a pane. When it was the one shown, the one before it
-- comes forward, or the one after it when it was first; a pane has at least
-- the one tab, so taking the last is the caller closing the pane.
paneRemove :: Int -> Pane -> Pane
paneRemove key p
  | docKey (paneFront p) /= key =
      p {paneBefore = filter ((/= key) . docKey) (paneBefore p), paneAfter = filter ((/= key) . docKey) (paneAfter p)}
  | prev : rest <- paneBefore p = p {paneFront = prev, paneBefore = rest}
  | next : rest <- paneAfter p = p {paneFront = next, paneAfter = rest}
  | otherwise = p

-- | A pane's tabs with one of theirs moved to this place among the others,
-- in front: what landing a held tab in a strip leaves it with.
panePlaced :: Int -> Doc -> Pane -> Pane
panePlaced index d p =
  let others = filter ((/= docKey d) . docKey) (paneDocs p)
      (before, after) = splitAt (max 0 (min index (length others))) others
   in p {paneFront = d, paneBefore = reverse before, paneAfter = after}

-- | Move the tab of this key to the place given among the others, in front.
paneMoveTo :: Int -> Int -> Pane -> Pane
paneMoveTo index key p = maybe p (\d -> panePlaced index d p) (paneDoc key p)

--------------------------------------------------------------------------------
-- The tabs
--------------------------------------------------------------------------------

-- | Every tab of every pane, the pane in front first.
appDocs :: App -> [Doc]
appDocs a = concatMap paneDocs (appAllPanes a)

-- | Whether there is more than the one tab or the one pane to put a tab in.
manyTabs :: App -> Bool
manyTabs a = length (appDocs a) > 1 || not (null (appPanes a))

-- | The tab holding a file, if one does.
tabWith :: FilePath -> App -> IO (Maybe Doc)
tabWith path app = (`findTab` app) <$> makeAbsolute path

-- | The tab holding a file, by its absolute path.
findTab :: FilePath -> App -> Maybe Doc
findTab path = find (\doc -> isNothing (docMarkdownPreviewOf doc) && maybe False (equalFilePath path) (docPath doc)) . appDocs

-- | The tab in front, gathered up from the record.
activeDoc :: App -> Doc
activeDoc app = Doc (appDocKey app) (appEditor app) (appPath app) (appFormat app) (appMarkdownPreviewOf app)

-- | Put a tab in front, in the place of the one that was, which goes nowhere:
-- the caller has put it somewhere already, or means to lose it.
showDoc :: Doc -> App -> App
showDoc doc app = case docMarkdownPreviewOf doc of
  Just sourceKey ->
    let source = fromMaybe doc (find ((== sourceKey) . docKey) (appDocs app))
        keepLinkFocus = appFocusMarkdownLinks app == Just sourceKey
     in app
          { appEditor = docEditor source
          , appPath = docPath source
          , appFormat = docFormat source
          , appDocKey = docKey doc
          , appMarkdownPreviewOf = Just sourceKey
          , appMarkdownPreview = hasMarkdownPreview sourceKey app
          , appFocusMarkdownLinks = if keepLinkFocus then appFocusMarkdownLinks app else Nothing
          , appRequestMarkdownLinkFocus = if keepLinkFocus then appRequestMarkdownLinkFocus app else Nothing
          , appStatus = if appFocusMarkdownLinks app /= Nothing && not keepLinkFocus then "Ready" else appStatus app
          }
  Nothing ->
    let changed = docKey doc /= appDocKey app
        keepLinkFocus = appFocusMarkdownLinks app == Just (docKey doc)
     in app
          { appEditor = docEditor doc
          , appPath = docPath doc
          , appFormat = docFormat doc
          , appDocKey = docKey doc
          , appMarkdownPreviewOf = Nothing
          , appMarkdownPreview = hasMarkdownPreview (docKey doc) app
          , appRequestMarkdownPreview = if changed then Nothing else appRequestMarkdownPreview app
          , appFocusMarkdownLinks = if keepLinkFocus then appFocusMarkdownLinks app else Nothing
          , appRequestMarkdownLinkFocus = if keepLinkFocus then appRequestMarkdownLinkFocus app else Nothing
          , appStatus = if appFocusMarkdownLinks app /= Nothing && not keepLinkFocus then "Ready" else appStatus app
          }

hasMarkdownPreview :: Int -> App -> Bool
hasMarkdownPreview sourceKey = any ((== Just sourceKey) . docMarkdownPreviewOf) . appDocs

-- | The tab in front put behind, before where the next one will go.
pushActive :: App -> App
pushActive app = app {appBefore = activeDoc app : appBefore app}

-- | Bring the tab of this key to the front, in the pane that has it, which
-- comes to the front with it. A key no tab has changes nothing.
selectDoc :: Int -> App -> App
selectDoc key a = case paneOfDoc key a of
  Just p -> focusPane (paneKey p) (putPane (paneSelect key p) a)
  Nothing -> a

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
      , docMarkdownPreviewOf = Nothing
      }
    (if replace then app else pushActive app) {appNextKey = appNextKey app + 1}
  where
    front = appEditor app

-- | Close the tab of this key, whatever it holds, in whatever pane it is.
-- Closing the tab in front brings the one before it forward, or the one after
-- it when it was first. A pane's last tab closes the pane, its place going to
-- the pane beside it; closing the last tab of the last pane leaves an
-- untitled one, since there is always a file to type into.
closeDoc :: Int -> App -> App
closeDoc key a =
  let previewKeys = [docKey doc | doc <- appDocs a, docMarkdownPreviewOf doc == Just key]
      sourceKeys = mapMaybe docMarkdownPreviewOf [doc | doc <- appDocs a, docKey doc == key]
      removed = foldl' (flip closeDocOne) a previewKeys
      closed = closeDocOne key removed
      cache = foldr Map.delete (appMarkdownCache closed) (key : previewKeys <> sourceKeys)
   in (refreshMarkdownPreviewState closed) {appMarkdownCache = cache}

closeDocOne :: Int -> App -> App
closeDocOne key a = case paneOfDoc key a of
  Just p
    | length (paneDocs p) > 1 -> onPane (paneKey p) (paneRemove key) a
    | otherwise -> closePane (paneKey p) a
  Nothing -> a

hasSeparateMarkdownPreview :: App -> Bool
hasSeparateMarkdownPreview = isJust . separateMarkdownPreview

mergeMarkdownPreview :: App -> App
mergeMarkdownPreview app = case separateMarkdownPreview app of
  Just (preview, sourcePane, previewPane) ->
    moveDocAcross
      (paneKey previewPane)
      (docKey preview)
      (paneKey sourcePane)
      (length (paneDocs sourcePane))
      app
  Nothing -> app

separateMarkdownPreview :: App -> Maybe (Doc, Pane, Pane)
separateMarkdownPreview app = do
  let sourceKey = fromMaybe (appDocKey app) (appMarkdownPreviewOf app)
  preview <- find ((== Just sourceKey) . docMarkdownPreviewOf) (appDocs app)
  sourcePane <- paneOfDoc sourceKey app
  previewPane <- paneOfDoc (docKey preview) app
  if paneKey sourcePane == paneKey previewPane
    then Nothing
    else Just (preview, sourcePane, previewPane)

-- | Take a whole pane away: its tabs from the others, and it from the row,
-- which its own pane does as it is drawn ('pgcClose'). Taking the one pane
-- there is leaves it an untitled tab, since there is always a file to type
-- into.
closePane :: Word64 -> App -> App
closePane pid a
  | null (appPanes a) = refreshMarkdownPreviewState ((newDoc a) {appBefore = []})
  | otherwise =
      let a1 = if pid == appPaneKey a then focusBeside pid a else a
          removedSourceKeys = maybe [] (mapMaybe docMarkdownPreviewOf . paneDocs) (paneOf pid a)
          closed = refreshMarkdownPreviewState $ a1
            { appPanes = filter ((/= pid) . paneKey) (appPanes a1)
            , appClosePane = Just pid
            }
       in closed {appMarkdownCache = foldr Map.delete (appMarkdownCache closed) removedSourceKeys}

refreshMarkdownPreviewState :: App -> App
refreshMarkdownPreviewState app =
  let sourceKey = fromMaybe (appDocKey app) (appMarkdownPreviewOf app)
      open = hasMarkdownPreview sourceKey app
      sourceExists = any ((== sourceKey) . docKey) (appDocs app)
      focus = if open then appFocusMarkdownLinks app else Nothing
   in app
        { appMarkdownPreview = open
        , appFocusMarkdownLinks = focus
        , appRequestMarkdownLinkFocus = if isJust focus then appRequestMarkdownLinkFocus app else Nothing
        , appRequestMarkdownPreview = if open || sourceExists then appRequestMarkdownPreview app else Nothing
        }

-- | Give the pane in front's keyboard to a pane beside it: the one before it
-- among the panes there are, or the one after, round from the ends.
focusBeside :: Word64 -> App -> App
focusBeside pid a = maybe a (`focusPane` a) beside
  where
    ids = map paneKey (appAllPanes a)
    after = drop 1 (dropWhile (/= pid) ids)
    before = reverse (takeWhile (/= pid) ids)
    beside = listToMaybe (after <> before)

-- | Change how the text of every tab is shown. How big it is and whether its
-- indentation is marked are the reader's, not the file's, so they are the
-- same in every tab of every pane.
everyEditor :: (Editor -> Editor) -> App -> App
everyEditor f app =
  app
    { appEditor = f (appEditor app)
    , appBefore = map onDoc (appBefore app)
    , appAfter = map onDoc (appAfter app)
    , appPanes = map onPaneRec (appPanes app)
    }
  where
    onDoc d = d {docEditor = f (docEditor d)}
    onPaneRec p =
      p
        { paneFront = onDoc (paneFront p)
        , paneBefore = map onDoc (paneBefore p)
        , paneAfter = map onDoc (paneAfter p)
        }

-- | What a tab is called: its file's name, or "Untitled".
docName :: Doc -> Text
docName doc
  | isJust (docMarkdownPreviewOf doc) = "Preview"
  | otherwise = maybe "Untitled" (T.pack . takeFileName) (docPath doc)

-- | Whether a tab has changes to save.
docDirty :: Doc -> Bool
docDirty doc = isNothing (docMarkdownPreviewOf doc) && B.isDirty (edBuffer (docEditor doc))

--------------------------------------------------------------------------------
-- A tab on its way
--------------------------------------------------------------------------------

-- A strip holds its tab up when its own drag ('useDrag') takes the press
-- past the threshold, and says where a release would land it as the pointer
-- goes: into a strip at the place the strip's drag reports, or into a pane
-- beside it over its body -- and when nothing has said, the pane grid
-- proposes a pane of the tab's own, which the release commits. Landing is
-- the only thing that moves a tab between panes, so a drag given up on the
-- tree, the bars or the desktop moves nothing.

-- | The strip's drag holds its tab, from this pane, where the pointer is and
-- at this phase of the drag. The tab comes to the front with it, as a
-- browser takes a tab the moment it is pressed. A hold already going keeps
-- where a release would land it, which the panes say as the pointer goes.
holdTab :: Int -> Word64 -> V2 -> DragPhase -> App -> App
holdTab key pid pos phase a =
  (selectDoc key a)
    { appTabDrag = Just held
    , appTreeFocus = False
    , appBarFocus = False
    }
  where
    held = case appTabDrag a of
      Just h | htFrom h == pid, htDoc h == key -> h {htPos = pos, htPhase = phase}
      _ -> HeldTab key pid pos phase Nothing

-- | Once a frame, before the panes are drawn: nothing of where the held tab
-- would land, which the panes say again this frame.
rewindDrag :: App -> App
rewindDrag a = a {appTabDrag = (\d -> d {htDrop = Nothing}) <$> appTabDrag a}

-- | What the pane of this id would do with a release: a tab from another
-- pane, over its body, would move into it, after the one in front. What a
-- release would do over a strip, the strip's own drag says, and over the
-- pane a tab came from, the pane grid proposes; a strip that has said, this
-- pane's own or another's, is not said over.
noteDrop :: Word64 -> Rect -> App -> App
noteDrop pid pane a = case appTabDrag a of
  Just d
    | pid /= htFrom d
    , isNothing (htDrop d)
    , rectContains pane (htPos d)
    , Just p <- paneOf pid a ->
        a {appTabDrag = Just d {htDrop = Just (pid, 1 + length (paneBefore p))}}
  _ -> a

-- | The drag's release frame: put the tab where the panes said, into the
-- pane the grid made for it when they said nowhere, or back where it came
-- from. Any other frame of the drag is kept as it is; the drag itself is
-- over on the release either way, and a cancellation was put back where it
-- came from already.
landHeldTab :: Maybe Word64 -> App -> App
landHeldTab made a0 = case appTabDrag a0 of
  Just d | htPhase d == DragReleased -> (land d) {appTabDrag = Nothing}
  _ -> a0
  where
    land d = case htDrop d of
      Just (pid, i)
        | pid == htFrom d -> onPane pid (paneMoveTo i (htDoc d)) a0
        | otherwise -> moveDocAcross (htFrom d) (htDoc d) pid i a0
      Nothing -> maybe a0 (\np -> toNewPane (htFrom d) (htDoc d) np a0) made

-- | Take a tab out of the pane of this id. When it was the one shown, the
-- one before it comes forward, or the one after it when it was first. A pane
-- left with nothing beside another closes; the pane in front is never left
-- with nothing, because the caller has made some other pane the one in front
-- first.
liftDocFrom :: Word64 -> Int -> App -> (Maybe Doc, App)
liftDocFrom pid key a = case paneOf pid a of
  Nothing -> (Nothing, a)
  Just p
    | [d] <- paneDocs p, pid /= appPaneKey a -> (Just d, closePane pid a)
    | otherwise -> (paneDoc key p, onPane pid (paneRemove key) a)

-- | Move a tab into another pane's strip, at the place given, in front. The
-- pane it came from keeps the rest of its tabs, or closes when that was its
-- last one.
moveDocAcross :: Word64 -> Int -> Word64 -> Int -> App -> App
moveDocAcross src key pid index a0
  | src /= pid =
      let (md, a2) = liftDocFrom src key (focusPane pid a0)
       in maybe a2 (\d -> onPane pid (panePlaced index d) a2) md
  | otherwise = a0

-- | The pane grid has made a pane for a tab dropped off the strips
-- ('commitPaneDrop'): give the tab to it, out of the pane it came from, and
-- put the keyboard there. The pane it came from keeps the rest of its tabs,
-- or closes when that was its last one, the grid closing up the row behind
-- it.
toNewPane :: Word64 -> Int -> Word64 -> App -> App
toNewPane src key npid a0 = case paneOf src a0 >>= paneDoc key of
  Nothing -> a0
  Just doc ->
    let a1 = a0 {appPanes = Pane npid doc [] [] : appPanes a0}
     in snd (liftDocFrom src key (focusPane npid a1))

--------------------------------------------------------------------------------
-- What follows from the state
--------------------------------------------------------------------------------

-- | Whether a modal is up: the question about unsaved changes, the finder, or
-- the command palette. While one is, it is the only thing that reads a key.
modalUp :: App -> Bool
modalUp app = isJust (appPending app) || isJust (appPicker app) || isJust (appCommandPalette app)

-- | What the editor marks the matches of: what the find bar holds, while it
-- is up.
findMarks :: App -> (B.Matching, Text)
findMarks app = (appFindMatching app, if appBar app == BarFind then appFindText app else "")

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

-- | Where the caret is: in which tab, at which version of its text, and where
-- in it.
hoverAt :: App -> (Int, Int, Int)
hoverAt app = (appDocKey app, B.bufVersion b, B.bufCursor b)
  where
    b = edBuffer (appEditor app)

-- | What the servers last said is wrong in the file in front.
frontDiagnostics :: App -> [Diagnostic]
frontDiagnostics app = maybe [] (\p -> Map.findWithDefault [] p (appDiagnostics app)) (appPath app)

-- | What the editor underlines: where each of those runs in the text, and
-- how bad it is. A diagnostic with nothing in its range marks a character.
diagnosticSpans :: App -> [(Int, Int, Int)]
diagnosticSpans app =
  [ (i, max (i + 1) (lspOffset b (diagEnd d)), diagSeverity d)
  | d <- frontDiagnostics app
  , let i = lspOffset b (diagStart d)
  ]
  where
    b = edBuffer (appEditor app)

-- | The offset in the text of a server's line and UTF-16 column. The text
-- may have changed since the server looked, so both are kept to what is
-- there.
lspOffset :: B.Buffer -> (Int, Int) -> Int
lspOffset b (l, c) = B.lineStart b l' + min (B.lineLength b l') (fromUtf16 (B.lineText b l') c)
  where
    l' = max 0 (min (B.lineCount b - 1) l)

-- | The name of the file in front, starred while it has changes to save.
titleFor :: App -> Text
titleFor app =
  docName (activeDoc app)
    <> (if docDirty (activeDoc app) then " *" else "")
    <> " - ned"
