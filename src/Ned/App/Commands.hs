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
  , vimChords

    -- * The bar, the tree and the finder
  , openBar
  , closeBar
  , findMatch
  , zoom
  , resetZoom

    -- * Settings
  , takeReading
  , fontFor
  , toggleTree
  , openPicker
  , focusToward

    -- * Vim
  , runVimRequests
  ) where

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
import qualified NanoUI.Shortcut as K
import Ned.App.State
import Ned.Buffer (Buffer)
import Ned.Config (Config (..), Font (..), Reading)
import qualified Ned.Buffer as B
import Ned.Editor
import qualified Ned.Editor.Vim as V
import Ned.File
import Ned.FileTree (FileTree)
import qualified Ned.FileTree as FT
import Ned.Highlight (LexState (..), languageFor)
import qualified Ned.Picker as P
import System.Exit (exitSuccess)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr)

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

-- | The chords that are vim's with vim's keys on, and not the application's:
-- Ctrl+N and Ctrl+P complete a word in insert mode and move down and up in
-- normal mode, and Ctrl+W deletes the word before the caret. What they did
-- here is on the File menu still, and on the leader and the command line.
vimChords :: [K.Shortcut]
vimChords = [chordNew, chordFindFile, chordCloseTab]

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
  let size = clampFontSize (f (edFontSize (appEditor a)))
   in everyEditor (\ed -> ed {edFontSize = size}) a

-- | Put the text back at the size the settings start it at.
resetZoom :: IORef App -> NanoUI ()
resetZoom ref = readApp ref >>= zoom ref . const . cfgBufferFontSize . appConfig

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

-- | Take up the settings read again while the window is open. A setting the
-- file changed is applied; one it left alone stays as it has been set since,
-- from the View menu or the zoom. A file that would not read changes nothing.
-- What went wrong is written to the terminal in full, as it is when the
-- window opens.
takeReading :: IORef App -> Reading -> NanoUI ()
takeReading ref = \case
  Left err -> do
    liftIO (hPutStrLn stderr err)
    setStatus ref "The settings were not reloaded; see the terminal"
  Right (new, problem) -> do
    old <- appConfig <$> readApp ref
    liftIO (mapM_ (hPutStrLn stderr) problem)
    let changed f = f old /= f new
        follow f cur = if changed f then f new else cur
        editor ed =
          ed
            { edFontSize = follow cfgBufferFontSize (edFontSize ed)
            , edShowWhitespace = follow cfgShowIndentation (edShowWhitespace ed)
            , edVim = if changed cfgVimKeys then (if cfgVimKeys new then Just V.newVim else Nothing) else edVim ed
            }
        -- The chrome's text size and the text's font are set when the
        -- window opens and cannot be changed under it.
        later = [name | (name, True) <- [("uiFontSize", changed cfgUiFontSize), ("bufferFont", changed cfgBufferFont)]]
        status
          | Just _ <- problem = "Some reloaded settings were not used; see the terminal"
          | null later = "Settings reloaded"
          | otherwise = "Settings reloaded; " <> T.intercalate " and " later <> " take effect on restart"
    when (changed cfgScale) (setSdlUiScale (cfgScale new))
    when (changed cfgUiFont) (setSdlUiFont (fontFor (sdlAppFont defaultSdlOptions) (cfgUiFont new)))
    modifyApp ref $ \a ->
      let shown = follow cfgShowFileTree (appTreeShown a)
       in everyEditor editor a {appConfig = new, appTreeShown = shown, appTreeFocus = appTreeFocus a && shown, appStatus = status}
    requestFrame

-- | A font from the settings: a file is loaded as it is, and a family is
-- looked for ahead of the ones nano-ui looks for, which are still there to
-- fall back on when it is not installed.
fontFor :: NanoUIFont -> Maybe Font -> NanoUIFont
fontFor fallback = \case
  Nothing -> fallback
  Just (FontFile path) -> FontFilePath path
  Just (FontFamily name)
    | FontSearch names <- fallback -> FontSearch (name : names)
    | otherwise -> FontSearch [name]

-- | Put the tree away or bring it back. Putting it away hands the keyboard
-- back to the editor.
toggleTree :: IORef App -> NanoUI ()
toggleTree ref = modifyApp ref $ \a -> a {appTreeShown = not (appTreeShown a), appTreeFocus = False}

-- | Give the keyboard to the pane on one side of the one that has it, by
-- vim's letter for the side: the tree is left of the text, and the bar is
-- under both. There being nothing that side, the keyboard stays.
focusToward :: IORef App -> Char -> NanoUI ()
focusToward ref side = modifyApp ref $ \a ->
  let toTree = a {appTreeFocus = True, appBarFocus = False}
      toText = a {appTreeFocus = False, appBarFocus = False}
      toBar = a {appTreeFocus = False, appBarFocus = True}
   in case side of
        'h' | appTreeShown a && not (appBarFocus a) -> toTree
        'l' | appTreeFocus a -> toText
        'j' | appBar a /= BarNone -> toBar
        'k' | appBarFocus a -> toText
        _ -> a

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

--------------------------------------------------------------------------------
-- Vim
--------------------------------------------------------------------------------

-- | Do what vim's keys asked of the application. They are done a frame after
-- they were asked for, at its start, so that the keys that put up the finder
-- or the find bar are not typed into it as well.
runVimRequests :: IORef App -> NanoUI ()
runVimRequests ref = do
  a <- readApp ref
  for_ (edVim (appEditor a)) $ \v ->
    unless (null (V.vimRequests v)) $ do
      onEditor ref (\ed -> ed {edVim = Just v {V.vimRequests = []}})
      mapM_ (vimRequest ref) (V.vimRequests v)

vimRequest :: IORef App -> V.Request -> NanoUI ()
vimRequest ref = \case
  V.FindFile -> openPicker ref P.fileSource
  V.Grep -> openPicker ref P.grepSource
  V.ToggleTree -> toggleTree ref
  V.FindBar -> openBar ref BarFind
  -- Vim's caret is on the start of the match it went to, not selecting
  -- it: the character it is on stands for the match, to look past.
  V.FindAgain forward -> do
    onBuffer ref (\b -> if B.hasSelection b then b else B.setCursor True (B.bufCursor b + 1) b)
    findMatch ref forward
  V.Save -> save ref False
  V.Quit force -> do
    a <- readApp ref
    let action = if manyTabs a then PendingClose (appDocKey a) else PendingQuit
    -- A save that put up its dialog (:wq on a file with no name) has not
    -- saved yet, and closing would only ask to throw away what it will.
    unless (isJust (appSaveDlg a) && not force) (run force action)
  V.QuitAll force -> run force PendingQuit
  V.NextTab forward -> stepTab ref forward
  V.Message msg -> setStatus ref msg
  where
    run force = if force then runPending ref else guarded ref
