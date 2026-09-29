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
  , newFileIn
  , landHeldTabUi
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
  , chordCommandPalette
  , chordMarkdownPreview
  , chordZoomIn
  , chordZoomOut
  , chordZoomReset
  , chordDefinition
  , vimChords

    -- * The bar, the tree and the finder
  , openBar
  , closeBar
  , findMatch
  , findPast
  , confirmFind
  , zoom
  , resetZoom

    -- * Settings
  , takeReading
  , fontFor
  , toggleTree
  , openCommandPalette
  , toggleMarkdownPreview
  , moveMarkdownPreviewIntoTabs
  , PaletteCommand (..)
  , commandPaletteCommands
  , openPicker
  , focusToward

    -- * Language servers
  , gotoDefinition
  , showHover
  , jumpDiagnostic
  , syncServer
  , takeAnswers

    -- * Vim
  , runVimRequests
  , queueVimRequest
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (foldM, unless, void, when)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', readIORef, writeIORef)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Data.Text.NanoRope.Measured as Rope
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Markdown (parseMarkdown)
import qualified NanoUI.Shortcut as K
import Ned.App.State
import Ned.Buffer (Buffer)
import Ned.Config (Config (..), FileSettings (..), Font (..), Project (..), Reading, settingsFor)
import qualified Ned.Buffer as B
import Ned.Editor
import qualified Ned.Editor.Vim as V
import Ned.File
import Ned.FileTree (FileTree)
import qualified Ned.FileTree as FT
import Ned.Highlight (LexState (..), langName, languageFor)
import qualified Ned.Lsp as Lsp
import qualified Ned.Picker as P
import System.Exit (exitSuccess)
import System.Directory (getHomeDirectory, makeAbsolute)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- The state
--------------------------------------------------------------------------------

readApp :: IORef App -> NanoUI App
readApp = liftIO . readIORef

modifyApp :: IORef App -> (App -> App) -> NanoUI ()
modifyApp ref change = liftIO $ modifyIORef' ref (syncPreview . change)
  where
    syncPreview app = case appMarkdownPreviewOf app of
      Nothing -> app
      Just sourceKey ->
        let sync doc
              | docKey doc == sourceKey =
                  doc
                    { docEditor = appEditor app
                    , docPath = appPath app
                    , docFormat = appFormat app
                    }
              | otherwise = doc
            syncPane pane =
              pane
                { paneFront = sync (paneFront pane)
                , paneBefore = map sync (paneBefore pane)
                , paneAfter = map sync (paneAfter pane)
                }
         in app
              { appBefore = map sync (appBefore app)
              , appAfter = map sync (appAfter app)
              , appPanes = map syncPane (appPanes app)
              }

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

-- | A new tab in the pane of this id, which comes to the front with it.
newFileIn :: IORef App -> Word64 -> NanoUI ()
newFileIn ref pid = modifyApp ref (focusPane pid) >> newFile ref

-- | The frame a held tab's drag releases on: land it where the strips and
-- panes said, and, when they said nowhere and the pane grid proposes a pane
-- for it, make that pane ('commitPaneDrop') and put the tab in it. Any other
-- frame of the drag is kept as it is.
landHeldTabUi :: IORef App -> Maybe PaneGridDrop -> NanoUI ()
landHeldTabUi ref target = do
  a <- readApp ref
  case appTabDrag a of
    Just d | htPhase d == DragReleased -> do
      made <- case htDrop d of
        Just _ -> pure Nothing
        Nothing -> traverse commitPaneDrop target
      modifyApp ref (landHeldTab (fmap fst =<< made))
    _ -> pure ()

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
  PendingNew -> modifyApp ref (\a -> (blankDoc a) {appStatus = "New file"})
  PendingQuit -> liftIO exitSuccess

-- | The tabs whose changes something would throw away.
losing :: App -> Pending -> [Doc]
losing a = \case
  PendingClose key -> filter (\d -> docKey d == key && docDirty d) (appDocs a)
  -- A file open already only has its tab brought forward.
  PendingReplace path
    | isJust (findTab path a) -> []
    | otherwise -> filter docDirty [activeDoc a]
  PendingNew -> filter docDirty [activeDoc a]
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
    (PendingNew, doc : _) -> (docName doc <> " has changes that are not saved.", "Discard them and start an untitled file in its place?")
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

chordDefinition :: K.Shortcut
chordDefinition = K.ctrl <> K.key ']'

chordCommandPalette :: K.Shortcut
chordCommandPalette = K.ctrl <> K.shift <> K.key 'p'

chordMarkdownPreview :: K.Shortcut
chordMarkdownPreview = K.ctrl <> K.shift <> K.key 'm'

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
   in showBar bar True a {appFindText = findText}

-- | A bar up under the text, with the keyboard or without it.
showBar :: Bar -> Bool -> App -> App
showBar bar focus a = a {appBar = bar, appBarFocus = focus, appGotoText = ""}

closeBar :: IORef App -> NanoUI ()
closeBar ref = modifyApp ref (\a -> a {appBar = BarNone, appBarFocus = False})

-- | Select the next match of what the find bar holds, or the previous one.
findMatch :: IORef App -> Bool -> NanoUI ()
findMatch ref forward = do
  a <- readApp ref
  let needle = appFindText a
      go = if forward then B.findNext else B.findPrev
  unless (T.null needle) $
    case go (appFindMatching a) needle (edBuffer (appEditor a)) of
      Just b -> onBuffer ref (const b) >> setStatus ref ""
      Nothing -> setStatus ref ("No match for " <> needle)

-- | The same, from vim's caret. It is on the start of the match it went to,
-- not selecting it: the character it is on stands for the match, to look
-- past.
findPast :: IORef App -> Bool -> NanoUI ()
findPast ref forward = do
  onBuffer ref (\b -> if B.hasSelection b then b else B.setCursor True (B.bufCursor b + 1) b)
  findMatch ref forward

-- | End vim's search: the caret stays on the match typed to, or goes on to
-- the next one when the query has not put it on one, and the keyboard goes
-- back to the text for n and N. Backward, it goes to the one before.
confirmFind :: IORef App -> Bool -> NanoUI ()
confirmFind ref back = do
  a <- readApp ref
  unless (not back && B.selectsMatch (appFindMatching a) (appFindText a) (edBuffer (appEditor a))) (findPast ref (not back))
  modifyApp ref (\a' -> a' {appBarFocus = False})

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

openCommandPalette :: IORef App -> NanoUI ()
openCommandPalette ref =
  modifyApp ref (\a -> a {appCommandPalette = Just (PaletteState "" 0)})

data PaletteCommand = PaletteCommand
  { paletteCommandName :: !Text
  , paletteCommandShortcut :: !Text
  , paletteCommandAction :: NanoUI ()
  }

commandPaletteCommands :: IORef App -> App -> [PaletteCommand]
commandPaletteCommands ref app =
  [ command "New File" "Ctrl+N" (newFile ref)
  , command "Open File..." "Ctrl+O" (openDialog ref)
  , command "Find File..." "Ctrl+P" (openPicker ref P.fileSource)
  , command "Save" "Ctrl+S" (save ref False)
  , command "Save As..." "Ctrl+Shift+S" (save ref True)
  , command "Close Tab" "Ctrl+W" (closeTab ref (appDocKey app))
  , command "Quit" "Ctrl+Q" (guarded ref PendingQuit)
  , command "Find..." "Ctrl+F" (openBar ref BarFind)
  , command "Search in Files..." "Ctrl+Shift+F" (openPicker ref P.grepSource)
  , command "Go to Line..." "Ctrl+G" (openBar ref BarGoto)
  , command "Go to Definition" "Ctrl+]" (gotoDefinition ref)
  , command "Toggle File Tree" "Ctrl+B" (toggleTree ref)
  , command "Zoom In" "Ctrl+= / Ctrl++" (zoom ref (* 1.1))
  , command "Zoom Out" "Ctrl+-" (zoom ref (/ 1.1))
  , command "Reset Zoom" "Ctrl+0" (resetZoom ref)
  , command "Toggle Indentation Marks" "" $
      modifyApp ref (everyEditor (\ed -> ed {edShowWhitespace = not (edShowWhitespace (appEditor app))}))
  , command "Toggle Vim Keys" "" $
      modifyApp ref (everyEditor (\ed -> ed {edVim = maybe (Just V.newVim) (const Nothing) (edVim (appEditor app))}))
  , command "Toggle Tabs and Spaces" "" $
      onBuffer ref (B.setUsesTabs (not (B.usesTabs (edBuffer (appEditor app)))))
  , command "Toggle Line Endings" "" $
      modifyApp ref (\a -> a {appFormat = (appFormat a) {formatEol = if formatEol (appFormat app) == LF then CRLF else LF}})
  ]
     <> ( if manyTabs app
            then
              [ command "Next Tab" "Ctrl+Tab" (stepTab ref True)
              , command "Previous Tab" "Ctrl+Shift+Tab" (stepTab ref False)
              ]
            else []
        )
    <> [ command "Undo" "Ctrl+Z" (onBuffer ref B.undo) | B.canUndo (edBuffer (appEditor app)) ]
    <> [ command "Redo" "Ctrl+Y" (onBuffer ref B.redo) | B.canRedo (edBuffer (appEditor app)) ]
    <> [ command "Cut" "Ctrl+X" (onBufferIO ref clipboardCut)
       , command "Copy" "Ctrl+C" (onBufferIO ref clipboardCopy)
       , command "Paste" "Ctrl+V" (onBufferIO ref clipboardPaste)
       , command "Select All" "Ctrl+A" (onBuffer ref B.selectAll)
       ]
     <> ( if langName (edLang (appEditor app)) == "Markdown"
             then
               [command "Toggle Markdown Preview" "Ctrl+Shift+M" (toggleMarkdownPreview ref)]
             else []
        )
     <> [command "Move Preview into Source Tabs" "" (moveMarkdownPreviewIntoTabs ref) | hasSeparateMarkdownPreview app]
     <> [command "Reveal Current File" "" (for_ (appPath app) (onTree ref . FT.reveal)) | isJust (appPath app)]
    <> [command "Open Parent Folder" "" (onTree ref FT.parentRoot) | FT.hasParentRoot (appTree app)]
    <> [ command "Refresh File Tree" "" (onTree ref FT.refresh)
       , command "Collapse File Tree" "" (onTree ref FT.collapseAll)
       ]
  where
    command = PaletteCommand

toggleMarkdownPreview :: IORef App -> NanoUI ()
toggleMarkdownPreview ref = do
  app <- readApp ref
  let sourceKey = fromMaybe (appDocKey app) (appMarkdownPreviewOf app)
      previewDoc = find ((== Just sourceKey) . docMarkdownPreviewOf) (appDocs app)
  case previewDoc of
    Just doc ->
      modifyApp ref $ \a ->
        let closed = closeDoc (docKey doc) a
         in closed
              { appStatus = "Markdown preview hidden"
              }
    Nothing
      | langName (edLang (appEditor app)) /= "Markdown" -> setStatus ref "Open a Markdown file to show its preview"
      | appRequestMarkdownPreview app == Just sourceKey ->
          modifyApp ref $ \a ->
            a
              { appMarkdownPreview = False
              , appRequestMarkdownPreview = Nothing
              , appStatus = "Markdown preview hidden"
              }
      | otherwise ->
          modifyApp ref $ \a ->
            a
              { appMarkdownPreview = True
              , appRequestMarkdownPreview = Just sourceKey
              , appStatus = "Markdown preview shown"
              }

moveMarkdownPreviewIntoTabs :: IORef App -> NanoUI ()
moveMarkdownPreviewIntoTabs ref = do
  app <- readApp ref
  when (hasSeparateMarkdownPreview app) $
    modifyApp ref $ \a ->
      (mergeMarkdownPreview a) {appStatus = "Preview moved into source tabs"}

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
-- Language servers
--------------------------------------------------------------------------------

-- | Open where what is under the caret is defined, with the caret on it.
gotoDefinition :: IORef App -> NanoUI ()
gotoDefinition ref = askServer ref $ \s path pos ->
  Lsp.definition s path pos <&> \case
    Nothing -> \a -> pure a {appStatus = "No definition found"}
    Just (file, (l, c)) -> \a -> do
      a' <- openPath InNewTab (Just l) file a
      let ed = appEditor a'
          b = edBuffer ed
          col = Lsp.fromUtf16 (B.lineText b l) c
      pure a' {appEditor = revealCaret ed {edBuffer = B.setCursor False (B.lineStart b l + col) b}}

-- | Show what the language server says of what is under the caret, by the
-- caret, unless it has moved by the time the server answers.
showHover :: IORef App -> NanoUI ()
showHover ref = do
  at <- hoverAt <$> readApp ref
  askServer ref $ \s path pos ->
    Lsp.hover s path pos <&> \r a -> pure $ case r of
      Nothing -> a {appStatus = "Nothing to show here"}
      Just md -> a {appStatus = "", appHover = Just (at, TipDoc (parseMarkdown md))}

-- | Ask the language server for the file in front about the place of its
-- caret, on a thread of its own: the server may take a while, starting the
-- first time especially. What the answer does to the application is left
-- for the next frame, which the thread wakes, to do.
askServer :: IORef App -> (Lsp.Server -> FilePath -> (Int, Int) -> IO (App -> IO App)) -> NanoUI ()
askServer ref ask = do
  a <- readApp ref
  let ed = appEditor a
      b = edBuffer ed
      lang = langName (edLang ed)
      l = B.lineOf b (B.bufCursor b)
      col = Lsp.toUtf16 (B.lineText b l) (B.bufCursor b - B.lineStart b l)
  case appPath a of
    Nothing -> setStatus ref "Save the file before asking its language server"
    Just path ->
      case serverCommand (appConfig a) (FT.ftRoot (appTree a)) lang path of
        Nothing -> setStatus ref ("No language server for " <> lang)
        Just (cmd, root) -> do
          setStatus ref "Asking the language server..."
          post <- answerer ref
          let text = Rope.toText (B.bufRope b)
          void . liftIO . forkIO $ do
            r <- try $ do
              s <- startServer a post cmd root
              Lsp.syncDoc s path (Lsp.languageId lang) text
              ask s path (l, col)
            post (either serverFailed id r)

-- | Tell the language server of the file in front what it holds, each time
-- that changes, so that what it finds wrong in it follows the typing. It is
-- told on a thread that only ever tells it the latest, and a server that
-- would not start is left alone until it is asked something outright.
syncServer :: IORef App -> NanoUI ()
syncServer ref = do
  a <- readApp ref
  let ed = appEditor a
      b = edBuffer ed
      lang = langName (edLang ed)
  for_ (appPath a) $ \path -> when (appSynced a /= (path, B.bufVersion b)) $ do
    modifyApp ref (\a' -> a' {appSynced = (path, B.bufVersion b)})
    post <- answerer ref
    for_ (serverCommand (appConfig a) (FT.ftRoot (appTree a)) lang path) (\(cmd, root) ->
      liftIO . Lsp.handLatest (appSync a) $ do
        gone <- Lsp.didNotStart (appServers a) cmd root
        unless gone $
          try (startServer a post cmd root >>= \s -> Lsp.syncDoc s path (Lsp.languageId lang) (Rope.toText (B.bufRope b)))
            >>= either (post . serverFailed) pure)

-- | Go to the next thing the language server found wrong in the file in
-- front, or the one before, round from the ends, and say what it is: in
-- the status bar, and in full by the caret, with anything else found at the
-- same place, which the jump would otherwise pass over.
jumpDiagnostic :: IORef App -> Bool -> NanoUI ()
jumpDiagnostic ref forward = do
  a <- readApp ref
  let b = edBuffer (appEditor a)
      here = B.bufCursor b
      found = sortOn fst [(lspOffset b (Lsp.diagStart d), d) | d <- frontDiagnostics a]
      next
        | forward = listToMaybe (filter ((> here) . fst) found <> found)
        | otherwise = listToMaybe (reverse (filter ((< here) . fst) found) <> reverse found)
  case next of
    Nothing -> setStatus ref "No diagnostics"
    Just (at, d) -> do
      onBuffer ref (B.setCursor False at)
      let there = sortOn Lsp.diagSeverity [d' | (i, d') <- found, i == at]
      modifyApp ref (\a' -> a' {appHover = Just (hoverAt a', TipDiagnostics there)})
      let kind = Lsp.severityName (Lsp.diagSeverity d)
          msg = fromMaybe "" (find (not . T.null) (map T.strip (T.lines (Lsp.diagMessage d))))
      setStatus ref (kind <> ": " <> msg)

-- | Start the server for a command line in a folder, or find it running,
-- with what it finds wrong going to the application by @post@.
startServer :: App -> ((App -> IO App) -> IO ()) -> [String] -> FilePath -> IO Lsp.Server
startServer a post = Lsp.serverFor (appServers a) $ \path ds ->
  post (\a' -> pure a' {appDiagnostics = Map.insert path ds (appDiagnostics a')})

serverFailed :: SomeException -> App -> IO App
serverFailed e a = pure a {appStatus = "Language server: " <> T.pack (displayException e)}

-- | How a thread hands the application something to do: it is queued for
-- the next frame, which it wakes.
answerer :: IORef App -> NanoUI ((App -> IO App) -> IO ())
answerer ref = do
  answers <- appAnswers <$> readApp ref
  wake <- askWake
  pure (\f -> atomicModifyIORef' answers (\fs -> (fs ++ [f], ())) >> wake)

-- | The command line a file's language server is run by, as the file's
-- settings have it, and the folder it is run in: the root of the project
-- the file is in, or the tree's folder when it is in none.
serverCommand :: Config -> FilePath -> Text -> FilePath -> Maybe ([String], FilePath)
serverCommand cfg treeRoot lang path =
  lookup (T.toLower lang) [(T.toLower name, c) | (name, c) <- fsLanguageServers fs] <&> \c ->
    (map T.unpack (fsServerShell fs) <> [T.unpack c], maybe treeRoot projRoot project)
  where
    (fs, project) = settingsFor cfg path

-- | Do what the language servers' answers ask, in the order they came.
takeAnswers :: IORef App -> NanoUI ()
takeAnswers ref = liftIO $ do
  a <- readIORef ref
  fs <- atomicModifyIORef' (appAnswers a) ([],)
  unless (null fs) (foldM (&) a fs >>= writeIORef ref)

--------------------------------------------------------------------------------
-- Vim
--------------------------------------------------------------------------------

-- | Do what vim's keys asked of the application. They are done a frame after
-- they were asked for, at its start, so that the keys that put up the finder
-- or the find bar are not typed into it as well. What the file tree's vim
-- keys asked goes in the same queue, since those keys mean as much there.
runVimRequests :: IORef App -> NanoUI ()
runVimRequests ref = do
  a <- readApp ref
  let asked = appRequests a ++ maybe [] V.vimRequests (edVim (appEditor a))
  unless (null asked) $ do
    for_ (edVim (appEditor a)) $ \v ->
      onEditor ref (\ed -> ed {edVim = Just v {V.vimRequests = []}})
    modifyApp ref (\a' -> a' {appRequests = []})
    mapM_ (vimRequest ref) asked

-- | Put what vim's keys asked of the application in the queue, for
-- 'runVimRequests' to do at the start of the next frame.
queueVimRequest :: IORef App -> V.Request -> NanoUI ()
queueVimRequest ref r = modifyApp ref (\a -> a {appRequests = appRequests a ++ [r]})

vimRequest :: IORef App -> V.Request -> NanoUI ()
vimRequest ref = \case
  V.FindFile -> openPicker ref P.fileSource
  V.Grep -> openPicker ref P.grepSource
  V.ToggleTree -> toggleTree ref
  -- Vim's / looks for text wherever it is, not only as a whole word.
  V.FindBar -> do
    modifyApp ref (\a -> a {appFindMatching = (appFindMatching a) {B.matchWord = False}})
    openBar ref BarFind
  V.FindAgain forward -> findPast ref forward
  -- The bar is put up to show what is marked, and the keyboard stays with
  -- the text for n and N.
  V.Search needle whole forward -> do
    modifyApp ref (\a -> showBar BarFind False a {appFindText = needle, appFindMatching = (appFindMatching a) {B.matchWord = whole}})
    findPast ref forward
  V.Save -> save ref False
  V.Quit force -> do
    a <- readApp ref
    let action = if manyTabs a then PendingClose (appDocKey a) else PendingQuit
    -- A save that put up its dialog (:wq on a file with no name) has not
    -- saved yet, and closing would only ask to throw away what it will.
    unless (isJust (appSaveDlg a) && not force) (run force action)
  V.QuitAll force -> run force PendingQuit
  V.NextTab forward -> stepTab ref forward
  V.Edit path force -> liftIO (expandPath path) >>= run force . PendingReplace
  V.Revert -> revert ref
  V.NewFile force -> run force PendingNew
  V.NewTab Nothing -> newFile ref
  V.NewTab (Just path) -> liftIO (expandPath path) >>= openFile ref Nothing
  V.ClearFind -> do
    a <- readApp ref
    when (appBar a == BarFind) (closeBar ref)
  V.Definition -> gotoDefinition ref
  V.Hover -> showHover ref
  V.NextDiagnostic forward -> jumpDiagnostic ref forward
  V.Message msg -> setStatus ref msg
  where
    run force = if force then runPending ref else guarded ref

-- | A file named on the command line, from the folder ned was started in,
-- with a @~@ at its start for the home folder.
expandPath :: FilePath -> IO FilePath
expandPath = \case
  "~" -> getHomeDirectory
  '~' : '/' : rest -> (</> rest) <$> getHomeDirectory
  path -> makeAbsolute path

-- | Read the file in front from disk again, in place of what the tab holds.
-- It is one step of the history, so what it threw away is an undo away.
revert :: IORef App -> NanoUI ()
revert ref = do
  a <- readApp ref
  case appPath a of
    Nothing -> setStatus ref "No file name"
    Just path ->
      liftIO (loadFile path) >>= \case
        Left err -> setStatus ref ("Could not open " <> T.pack path <> ": " <> err)
        Right loaded -> modifyApp ref $ \a' ->
          let ed = appEditor a'
              b = edBuffer ed
              disk = Rope.toText (B.bufRope (loadedBuffer loaded))
              b'
                | Rope.toText (B.bufRope b) == disk = b
                | otherwise = B.setCursor False (min (B.bufCursor b) (T.length disk)) (B.replace 0 (B.size b) disk b)
           in a'
                { appEditor = revealCaret ed {edBuffer = B.markSaved b', edCompletion = Nothing}
                , appFormat = loadedFormat loaded
                , appStatus = "Read " <> T.pack (takeFileName path)
                }
