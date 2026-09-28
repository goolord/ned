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

    -- * The files
  , newFile
  , openDialog
  , openFile
  , openHere
  , closeTab
  , showTab
  , stepTab
  , runPending
  , guarded
  , pendingQuestion
  , save
  , saveTo
  , savePicked

    -- * The chords
  , chordNew
  , chordOpen
  , chordFindFile
  , chordSave
  , chordSaveAs
  , chordQuit
  , chordCloseTab
  , chordNextTab
  , chordPrevTab
  , chordFind
  , chordGrep
  , chordGoto
  , chordTree
  , chordZoomIn
  , chordZoomOut
  , chordZoomReset

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
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
import qualified NanoUI.Shortcut as K
import Ned.App.State
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor
import Ned.File
import Ned.FileTree (FileTree)
import qualified Ned.FileTree as FT
import Ned.Highlight (LexState (..), languageFor)
import qualified Ned.Picker as P
import Ned.Text (clamp)
import System.Exit (exitSuccess)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)

--------------------------------------------------------------------------------
-- The state
--------------------------------------------------------------------------------

readApp :: IORef App -> NanoUI App
readApp = liftIO . readIORef

modifyApp :: IORef App -> (App -> App) -> NanoUI ()
modifyApp ref = liftIO . modifyIORef' ref

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
-- The files
--------------------------------------------------------------------------------

-- | A new tab, untitled and empty.
newFile :: IORef App -> NanoUI ()
newFile ref = modifyApp ref (\a -> (newDoc a) {appStatus = "New file"})

-- | Ask the desktop for a file to open, starting in the folder of the one in
-- front.
openDialog :: IORef App -> NanoUI ()
openDialog ref = do
  a <- readApp ref
  dlg <- askOpenFileDialog defaultFileDialogOptions {dialogDefaultLocation = takeDirectory <$> appPath a}
  modifyApp ref (\a' -> a' {appOpenDlg = dlg})

-- | Open a file in a tab, or bring its tab forward, with the caret on a line
-- of it counted from zero, if one is given.
openFile :: IORef App -> Maybe Int -> FilePath -> NanoUI ()
openFile ref = openIn ref InNewTab

openIn :: IORef App -> Placement -> Maybe Int -> FilePath -> NanoUI ()
openIn ref placement line path = liftIO (readIORef ref >>= openPath placement line path >>= writeIORef ref)

-- | Open a file in the tab in front, in place of what it holds, asking first
-- when that has changes to lose; or bring its tab forward, if it is open.
openHere :: IORef App -> FilePath -> NanoUI ()
openHere ref path = liftIO (makeAbsolute path) >>= guarded ref . PendingReplace

-- | Close a tab, asking first when it has changes to lose.
closeTab :: IORef App -> Int -> NanoUI ()
closeTab ref key = guarded ref (PendingClose key)

-- | Bring a tab to the front.
showTab :: IORef App -> Int -> NanoUI ()
showTab ref key = modifyApp ref (selectDoc key)

-- | Bring the next tab to the front, or the one before.
stepTab :: IORef App -> Bool -> NanoUI ()
stepTab ref forward = modifyApp ref (stepDoc forward)

-- | Do something that throws text away, that having been agreed to.
runPending :: IORef App -> Pending -> NanoUI ()
runPending ref = \case
  PendingClose key -> modifyApp ref (closeDoc key)
  PendingReplace path -> openIn ref InFrontTab Nothing path
  PendingQuit -> liftIO exitSuccess

-- | The tabs whose changes something would throw away.
losing :: App -> Pending -> [Doc]
losing a = \case
  PendingClose key -> filter (\d -> docKey d == key && docDirty d) (appDocs a)
  -- A file open already only has its tab brought forward.
  PendingReplace path
    | isJust (findTab path a) -> []
    | otherwise -> filter docDirty [activeDoc a]
  PendingQuit -> filter docDirty (appDocs a)

-- | Ask first when there are changes to lose, and otherwise get on with it.
-- The tab being asked about is brought to the front, so that what the
-- question is about is there to see.
guarded :: IORef App -> Pending -> NanoUI ()
guarded ref action = do
  a <- readApp ref
  case losing a action of
    [] -> runPending ref action
    doc : _ -> modifyApp ref (\a' -> (selectDoc (docKey doc) a') {appPending = Just action})

-- | The question about unsaved changes, and what it asks to do about them.
pendingQuestion :: App -> Pending -> (Text, Text)
pendingQuestion a action =
  case (action, losing a action) of
    (PendingClose _, doc : _) -> (docName doc <> " has changes that are not saved.", "Discard them and close it?")
    (PendingReplace path, doc : _) -> (docName doc <> " has changes that are not saved.", "Discard them and open " <> T.pack (takeFileName path) <> " in its place?")
    (PendingQuit, [doc]) -> (docName doc <> " has changes that are not saved.", "Discard them and quit?")
    (PendingQuit, lost) -> (T.pack (show (length lost)) <> " files have changes that are not saved.", "Discard them and quit?")
    _ -> ("", "")

-- | Save; under a new name when the flag is set, or when there is no name.
save :: IORef App -> Bool -> NanoUI ()
save ref forceDialog = do
  a <- readApp ref
  case appPath a of
    Just path | not forceDialog -> saveTo ref path
    _ -> do
      dlg <- askSaveFileDialog defaultFileDialogOptions {dialogDefaultLocation = appPath a}
      modifyApp ref (\a' -> a' {appSaveDlg = (,appDocKey a) <$> dlg})

-- | Save the tab of this key, which a save dialog was put up for, under the
-- name picked in it, bringing it to the front, unless it was closed while
-- the dialog was up.
savePicked :: IORef App -> Int -> FilePath -> NanoUI ()
savePicked ref key path = do
  a <- readApp ref
  when (any ((== key) . docKey) (appDocs a)) $ do
    showTab ref key
    saveTo ref path

-- | Save the tab in front under a name, unless another tab holds that file:
-- the two would write over each other.
saveTo :: IORef App -> FilePath -> NanoUI ()
saveTo ref path = do
  a <- readApp ref
  other <- liftIO (tabWith path a)
  case other of
    Just d | docKey d /= appDocKey a -> setStatus ref (T.pack (takeFileName path) <> " is open in another tab; close it before saving over it")
    _ -> saveFront ref path

saveFront :: IORef App -> FilePath -> NanoUI ()
saveFront ref path = do
  a <- readApp ref
  liftIO (saveFile path (appFormat a) (edBuffer (appEditor a))) >>= \case
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
-- The chords
--------------------------------------------------------------------------------

-- | The chords of the application's commands, which "Ned.App.Frame" binds;
-- the menus name those of their rows beside them.
chordNew, chordOpen, chordFindFile, chordSave, chordSaveAs, chordQuit :: K.Shortcut
chordNew = K.ctrl <> K.key 'n'
chordOpen = K.ctrl <> K.key 'o'
chordFindFile = K.ctrl <> K.key 'p'
chordSave = K.ctrl <> K.key 's'
chordSaveAs = K.ctrl <> K.shift <> K.key 's'
chordQuit = K.ctrl <> K.key 'q'

chordCloseTab, chordNextTab, chordPrevTab :: K.Shortcut
chordCloseTab = K.ctrl <> K.key 'w'
chordNextTab = K.ctrl <> K.key KeyTab
chordPrevTab = K.ctrl <> K.shift <> K.key KeyTab

chordFind, chordGrep, chordGoto, chordTree, chordZoomIn, chordZoomOut, chordZoomReset :: K.Shortcut
chordFind = K.ctrl <> K.key 'f'
chordGrep = K.ctrl <> K.shift <> K.key 'f'
chordGoto = K.ctrl <> K.key 'g'
chordTree = K.ctrl <> K.key 'b'
chordZoomIn = K.ctrl <> K.key '='
chordZoomOut = K.ctrl <> K.key '-'
chordZoomReset = K.ctrl <> K.key '0'

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
zoom ref f = modifyApp ref $ \a ->
  let size = clamp 8 48 (f (edFontSize (appEditor a)))
   in everyEditor (\ed -> ed {edFontSize = size}) a

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
    pk <- liftIO (P.openPicker source (FT.ftRoot (appTree a)))
    modifyApp ref (\a' -> a' {appPicker = Just pk})
