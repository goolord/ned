-- | Everything the application can be asked to do.
--
-- The menus, the chords, the buttons on the find bar and the file dialogs all
-- ask for the same handful of things -- save this, open that, find the next
-- one, put the tree away -- and each of them works the same way: read the
-- state, work out the next one, write it back.
--
-- Gathering them behind one record leaves the frame in "Ned.View" saying only
-- when each is asked for, and lets a menu row be written beside the bar it
-- hangs from rather than inside the frame that draws it. Nothing here draws.
module Ned.App.Commands
  ( Commands (..)
  , commands
  ) where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Context (Context)
import Ned.App.State
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor
import Ned.File
import Ned.FileTree (FileTree)
import qualified Ned.FileTree as FT
import Ned.Highlight (LexState (..), languageFor, plainText)
import qualified Ned.Picker as P
import Ned.Text (clamp)
import System.Directory (makeAbsolute)
import System.Exit (exitSuccess)
import System.FilePath (equalFilePath, takeDirectory, takeFileName)

-- | What the window's parts call on. Every one of these reads the state as it
-- stands rather than as the frame found it, so a menu row that follows a
-- chord in the same frame acts on what the chord left behind.
data Commands = Commands
  { cmdRead :: NanoUI App
  , cmdModify :: (App -> App) -> NanoUI ()
  , cmdStatus :: Text -> NanoUI ()
  , cmdOnEditor :: (Editor -> Editor) -> NanoUI ()
  , cmdOnBuffer :: (Buffer -> Buffer) -> NanoUI ()
  -- ^ Edit the text, and scroll the caret back into view.
  , cmdOnBufferIO :: (Context -> Buffer -> IO Buffer) -> NanoUI ()
  -- ^ The same, for the clipboard, which has to ask the host.
  , cmdOnTree :: (FileTree -> FileTree) -> NanoUI ()
  , cmdRun :: Pending -> NanoUI ()
  -- ^ Do something that replaces the text, the text having been agreed to go.
  , cmdGuarded :: Pending -> NanoUI ()
  -- ^ Ask first when there are changes to lose, and otherwise get on with it.
  , cmdSave :: Bool -> NanoUI ()
  -- ^ Save; under a new name when the flag is set, or when there is no name.
  , cmdSaveTo :: FilePath -> NanoUI ()
  , cmdOpenBar :: Bar -> NanoUI ()
  , cmdCloseBar :: NanoUI ()
  , cmdFind :: Bool -> NanoUI ()
  -- ^ The next match, or the previous one.
  , cmdZoom :: (Float -> Float) -> NanoUI ()
  , cmdToggleTree :: NanoUI ()
  , cmdOpenPicker :: P.Source -> NanoUI ()
  -- ^ Put the fuzzy finder up over what is under the tree's root.
  , cmdCloseMenu :: NanoUI ()
  }

-- | The commands over one application's state.
commands :: Context -> IORef App -> Commands
commands ctx ref =
  Commands
    { cmdRead = uiIO (readIORef ref)
    , cmdModify = modify
    , cmdStatus = status
    , cmdOnEditor = onEditor
    , cmdOnBuffer = onBuffer
    , cmdOnBufferIO = onBufferIO
    , cmdOnTree = onTree
    , cmdRun = run
    , cmdGuarded = guarded
    , cmdSave = save
    , cmdSaveTo = saveTo
    , cmdOpenBar = openBar
    , cmdCloseBar = closeBar
    , cmdFind = find
    , cmdZoom = zoom
    , cmdToggleTree = toggleTree
    , cmdOpenPicker = openPicker
    , cmdCloseMenu = closeMenu
    }
  where
    modify = uiIO . modifyIORef' ref
    onEditor f = modify (\a -> a {appEditor = f (appEditor a)})
    onBuffer f = onEditor (\ed -> revealCaret ed {edBuffer = f (edBuffer ed)})
    onBufferIO f = do
      a <- uiIO (readIORef ref)
      b <- uiIO (f ctx (edBuffer (appEditor a)))
      onBuffer (const b)
    closeMenu = modify (\a -> a {appOpenMenu = ""})
    status msg = modify (\a -> a {appStatus = msg})

    -- Run something that replaces the text, once the text may go.
    run = \case
      PendingNew -> modify $ \a ->
        a
          { appEditor = (newEditor plainText B.empty) {edFontSize = edFontSize (appEditor a)}
          , appPath = Nothing
          , appFormat = FileFormat LF False
          , appStatus = "New file"
          }
      PendingOpen -> do
        a <- uiIO (readIORef ref)
        dlg <- askOpenFileDialog defaultFileDialogOptions {dialogDefaultLocation = takeDirectory <$> appPath a}
        modify (\a' -> a' {appOpenDlg = dlg})
      PendingOpenPath path -> uiIO (readIORef ref >>= openPath path >>= writeIORef ref)
      -- The caret goes to the line only if the file did open: one that could
      -- not be read leaves the text that was there, and the caret in it.
      PendingOpenAt path ln -> do
        run (PendingOpenPath path)
        opened <- uiIO (makeAbsolute path)
        a <- uiIO (readIORef ref)
        when (maybe False (equalFilePath opened) (appPath a)) $
          onBuffer (B.gotoLine (ln + 1))
      PendingQuit -> uiIO exitSuccess
    guarded action = do
      a <- uiIO (readIORef ref)
      if B.isDirty (edBuffer (appEditor a))
        then modify (\a' -> a' {appPending = Just action})
        else run action

    saveTo path = do
      a <- uiIO (readIORef ref)
      uiIO (saveFile path (appFormat a) (edBuffer (appEditor a))) >>= \case
        Left err -> status ("Could not save " <> T.pack path <> ": " <> err)
        Right () -> modify $ \a' ->
          a'
            { appEditor =
                (appEditor a')
                  { edBuffer = B.markSaved (edBuffer (appEditor a'))
                  , edLang = languageFor path
                  , -- Worked out under the language the file had.
                    edLexCache = (-1, 0, LexNormal)
                  }
            , appPath = Just path
            , appStatus = "Saved " <> T.pack (takeFileName path)
            }
    save forceDialog = do
      a <- uiIO (readIORef ref)
      case appPath a of
        Just path | not forceDialog -> saveTo path
        _ -> do
          dlg <- askSaveFileDialog defaultFileDialogOptions {dialogDefaultLocation = appPath a}
          modify (\a' -> a' {appSaveDlg = dlg})

    openBar bar = modify $ \a ->
      let sel = B.selectedText (edBuffer (appEditor a))
          seeded = bar == BarFind && not (T.null sel) && not (T.any (== '\n') sel) && T.length sel <= 200
          findText = if seeded then sel else appFindText a
       in a
            { appBar = bar
            , appBarFocus = True
            , appFindText = findText
            , appGotoText = ""
            , appEditor = (appEditor a) {edFind = if bar == BarFind then findText else ""}
            }
    closeBar = modify $ \a ->
      a {appBar = BarNone, appBarFocus = False, appEditor = (appEditor a) {edFind = ""}}
    find forward = do
      a <- uiIO (readIORef ref)
      let ed = appEditor a
          needle = appFindText a
          go = if forward then B.findNext else B.findPrev
      unless (T.null needle) $
        case go (edFindExact ed) needle (edBuffer ed) of
          Just b -> onBuffer (const b) >> status ""
          Nothing -> status ("No match for " <> needle)
    zoom f = onEditor (\ed -> ed {edFontSize = clamp 8 48 (f (edFontSize ed))})
    onTree f = modify (\a -> a {appTree = f (appTree a)})
    -- Putting the tree away hands the keyboard back to the editor.
    toggleTree = modify $ \a ->
      a {appTreeShown = not (appTreeShown a), appTreeFocus = False}
    -- The finder looks through the folder the tree is on, which is the one
    -- the reader has said they are working in. It sets its own thread
    -- gathering as it is made, so this is back before the first file is
    -- found, and asking for a finder that is already up does nothing.
    openPicker source = do
      a <- uiIO (readIORef ref)
      case appPicker a of
        Just _ -> pure ()
        Nothing -> do
          pk <- uiIO (P.openPicker source (FT.ftRoot (appTree a)))
          modify (\a' -> a' {appPicker = Just pk})
