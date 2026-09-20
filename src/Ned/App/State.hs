-- | Everything the application is, between frames.
--
-- The editor and the file tree keep their own state; this is what is left
-- over and belongs to neither: which file is open and in what format, which
-- menu is down, which bar is up, what has the keyboard, and the change that
-- is waiting to be agreed to. A frame reads one of these and writes the next.
module Ned.App.State
  ( App (..)
  , Bar (..)
  , Pending (..)
  , newApp
  , newAppIn
  , openPath
  , titleFor
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Backend.Sdl (FileDialogId)
import qualified Ned.Buffer as B
import Ned.Editor
import Ned.File
import Ned.FileTree (FileTree)
import qualified Ned.FileTree as FT
import Ned.Highlight (languageFor, plainText)
import System.Directory (doesFileExist, getCurrentDirectory, makeAbsolute)
import System.FilePath (takeFileName)

--------------------------------------------------------------------------------
-- The state
--------------------------------------------------------------------------------

-- | The bar under the editor.
data Bar = BarNone | BarFind | BarGoto
  deriving (Eq)

-- | Something that would throw the text away, held until that is agreed to.
data Pending = PendingNew | PendingOpen | PendingOpenPath FilePath | PendingQuit

data App = App
  { appEditor :: !Editor
  , appPath :: !(Maybe FilePath)
  , appFormat :: !FileFormat
  , appStatus :: !Text
  , appOpenMenu :: !Text
  , appMenuSwallow :: !Text
  -- ^ The menu whose own button's press just closed it, until the release.
  , appOpenDlg :: !(Maybe FileDialogId)
  , appSaveDlg :: !(Maybe FileDialogId)
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
  }

newApp :: App
newApp =
  App
    { appEditor = newEditor plainText B.empty
    , appPath = Nothing
    , appFormat = FileFormat LF False
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
    , appTree = FT.newFileTree "."
    , appTreeShown = True
    , appTreeFocus = False
    }

-- | A fresh application with its tree on the directory the program was
-- started in, which is where 'newApp' cannot look.
newAppIn :: IO App
newAppIn = do
  cwd <- getCurrentDirectory
  pure newApp {appTree = FT.newFileTree cwd}

--------------------------------------------------------------------------------
-- Opening a file
--------------------------------------------------------------------------------

-- | Open a file in the application, or start a new one under a name that
-- does not exist yet.
openPath :: FilePath -> App -> IO App
openPath path0 app = do
  path <- makeAbsolute path0
  exists <- doesFileExist path
  let fresh buf format msg =
        app
          { appEditor = (newEditor (languageFor path) buf) {edFontSize = edFontSize (appEditor app)}
          , appPath = Just path
          , appFormat = format
          , appStatus = msg
          , -- The tree follows the file: it opens the folders down to it, and
            -- moves to the folder the file is in when it is somewhere else.
            appTree = FT.reveal path (appTree app)
          }
  if not exists
    then pure (fresh B.empty (FileFormat LF False) ("New file " <> T.pack (takeFileName path)))
    else
      loadFile path >>= \case
        Left err -> pure app {appStatus = "Could not open " <> T.pack path <> ": " <> err}
        Right loaded ->
          pure . fresh (loadedBuffer loaded) (loadedFormat loaded) $
            if loadedLossy loaded
              then "Opened " <> T.pack (takeFileName path) <> ", which is not UTF-8: its other bytes are shown as " <> T.singleton (toEnum 0xFFFD) <> " and saving will not bring them back"
              else "Opened " <> T.pack (takeFileName path)

--------------------------------------------------------------------------------
-- The window's title
--------------------------------------------------------------------------------

-- | The file's name, starred while it has changes to save.
titleFor :: App -> Text
titleFor app =
  maybe "Untitled" (T.pack . takeFileName) (appPath app)
    <> (if B.isDirty (edBuffer (appEditor app)) then " *" else "")
    <> " - ned"
