-- | Everything the application can be asked to do.
--
-- The menus, the chords, the buttons on the find bar and the file dialogs all
-- ask for the same handful of things -- save this, open that, find the next
-- one, put the tree away -- and each of them works the same way: read the
-- state, work out the next one, write it back. Each is here, over the 'IORef'
-- the state is kept in, so the frame in "Ned.View" is left saying only when
-- each is asked for, and a menu row is written beside the bar it hangs from
-- rather than inside the frame that draws it. Nothing here draws.
--
-- Every one of these reads the state as it stands rather than as the frame
-- found it, so a menu row that follows a chord in the same frame acts on what
-- the chord left behind.
module Ned.App.Commands
  ( -- * The state
    readApp
  , modifyApp
  , setStatus
  , onEditor
  , onBuffer
  , onBufferIO
  , onTree

    -- * The file
  , runPending
  , guarded
  , save
  , saveTo

    -- * The bar, the tree and the finder
  , openBar
  , closeBar
  , findMatch
  , zoom
  , toggleTree
  , openPicker
  ) where

import Control.Monad (unless, when)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
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
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory, takeFileName)

--------------------------------------------------------------------------------
-- The state
--------------------------------------------------------------------------------

readApp :: IORef App -> NanoUI App
readApp = uiIO . readIORef

modifyApp :: IORef App -> (App -> App) -> NanoUI ()
modifyApp ref = uiIO . modifyIORef' ref

setStatus :: IORef App -> Text -> NanoUI ()
setStatus ref msg = modifyApp ref (\a -> a {appStatus = msg})

onEditor :: IORef App -> (Editor -> Editor) -> NanoUI ()
onEditor ref f = modifyApp ref (\a -> a {appEditor = f (appEditor a)})

-- | Edit the text, and scroll the caret back into view.
onBuffer :: IORef App -> (Buffer -> Buffer) -> NanoUI ()
onBuffer ref f = onEditor ref (\ed -> revealCaret ed {edBuffer = f (edBuffer ed)})

-- | The same, for the clipboard, which has to ask the host.
onBufferIO :: IORef App -> (Buffer -> NanoUI Buffer) -> NanoUI ()
onBufferIO ref f = readApp ref >>= f . edBuffer . appEditor >>= onBuffer ref . const

onTree :: IORef App -> (FileTree -> FileTree) -> NanoUI ()
onTree ref f = modifyApp ref (\a -> a {appTree = f (appTree a)})

--------------------------------------------------------------------------------
-- The file
--------------------------------------------------------------------------------

-- | Do something that replaces the text, the text having been agreed to go.
runPending :: IORef App -> Pending -> NanoUI ()
runPending ref = \case
  PendingNew -> modifyApp ref $ \a ->
    a
      { appEditor = (newEditor plainText B.empty) {edFontSize = edFontSize (appEditor a)}
      , appPath = Nothing
      , appFormat = FileFormat LF False
      , appStatus = "New file"
      }
  PendingOpen -> do
    a <- readApp ref
    dlg <- askOpenFileDialog defaultFileDialogOptions {dialogDefaultLocation = takeDirectory <$> appPath a}
    modifyApp ref (\a' -> a' {appOpenDlg = dlg})
  PendingOpenPath line path -> uiIO (readIORef ref >>= openPath line path >>= writeIORef ref)
  PendingQuit -> uiIO exitSuccess

-- | Ask first when there are changes to lose, and otherwise get on with it.
guarded :: IORef App -> Pending -> NanoUI ()
guarded ref action = do
  a <- readApp ref
  if B.isDirty (edBuffer (appEditor a))
    then modifyApp ref (\a' -> a' {appPending = Just action})
    else runPending ref action

-- | Save; under a new name when the flag is set, or when there is no name.
save :: IORef App -> Bool -> NanoUI ()
save ref forceDialog = do
  a <- readApp ref
  case appPath a of
    Just path | not forceDialog -> saveTo ref path
    _ -> do
      dlg <- askSaveFileDialog defaultFileDialogOptions {dialogDefaultLocation = appPath a}
      modifyApp ref (\a' -> a' {appSaveDlg = dlg})

saveTo :: IORef App -> FilePath -> NanoUI ()
saveTo ref path = do
  a <- readApp ref
  uiIO (saveFile path (appFormat a) (edBuffer (appEditor a))) >>= \case
    Left err -> setStatus ref ("Could not save " <> T.pack path <> ": " <> err)
    Right () -> modifyApp ref $ \a' ->
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

--------------------------------------------------------------------------------
-- The bar, the tree and the finder
--------------------------------------------------------------------------------

-- | Put a bar up under the text. Find starts from the selection, when that is
-- a short run of one line.
openBar :: IORef App -> Bar -> NanoUI ()
openBar ref bar = modifyApp ref $ \a ->
  let sel = B.selectedText (edBuffer (appEditor a))
      seeded = bar == BarFind && not (T.null sel) && not (T.any (== '\n') sel) && T.length sel <= 200
      findText = if seeded then sel else appFindText a
   in a
        { appBar = bar
        , appBarFocus = True
        , appFindText = findText
        , appGotoText = ""
        }

closeBar :: IORef App -> NanoUI ()
closeBar ref = modifyApp ref (\a -> a {appBar = BarNone, appBarFocus = False})

-- | Select the next match of what the find bar holds, or the previous one.
findMatch :: IORef App -> Bool -> NanoUI ()
findMatch ref forward = do
  a <- readApp ref
  let ed = appEditor a
      needle = appFindText a
      go = if forward then B.findNext else B.findPrev
  unless (T.null needle) $
    case go (edFindExact ed) needle (edBuffer ed) of
      Just b -> onBuffer ref (const b) >> setStatus ref ""
      Nothing -> setStatus ref ("No match for " <> needle)

zoom :: IORef App -> (Float -> Float) -> NanoUI ()
zoom ref f = onEditor ref (\ed -> ed {edFontSize = clamp 8 48 (f (edFontSize ed))})

-- | Put the tree away or bring it back. Putting it away hands the keyboard
-- back to the editor.
toggleTree :: IORef App -> NanoUI ()
toggleTree ref = modifyApp ref $ \a -> a {appTreeShown = not (appTreeShown a), appTreeFocus = False}

-- | Put the fuzzy finder up over the folder the tree is on, which is the one
-- the reader has said they are working in. It sets its own thread gathering
-- as it is made, so this is back before the first file is found, and asking
-- for a finder that is already up does nothing.
openPicker :: IORef App -> P.Source -> NanoUI ()
openPicker ref source = do
  a <- readApp ref
  when (isNothing (appPicker a)) $ do
    pk <- uiIO (P.openPicker source (FT.ftRoot (appTree a)))
    modifyApp ref (\a' -> a' {appPicker = Just pk})
