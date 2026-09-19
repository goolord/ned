-- | The application around the editor widget: the menu bar, files, the find
-- and go-to-line bar, the status bar, and the chords that drive them.
module Ned.App
  ( runNed
  , App (..)
  , newApp
  , appView
  , openPath
  , loadFile
  , saveFile
  , Eol (..)
  , FileFormat (..)
  , Loaded (..)
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (Ptr, castPtr)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.NanoRope as Rope
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Context (Context (..), getFocusId, markDirty)
import NanoUI.Monad (askContext, askFrameInput, askHost, askInput)
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Highlight (Lang (..), LexState (..), languageFor, plainText)
import Ned.View
import GHC.Clock (getMonotonicTime)
import System.Directory (doesFileExist, makeAbsolute)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (IOMode (WriteMode), withBinaryFile)
import Text.Printf (printf)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

-- | How the file on disk ends its lines. The buffer always holds @\\n@.
data Eol = LF | CRLF
  deriving (Eq, Show)

-- | What of a file is not its text, and is put back when it is saved.
data FileFormat = FileFormat
  { formatEol :: !Eol
  , formatBom :: !Bool
  -- ^ Whether the file starts with a UTF-8 byte order mark.
  }
  deriving (Eq, Show)

-- | A file as read.
data Loaded = Loaded
  { loadedBuffer :: !Buffer
  , loadedFormat :: !FileFormat
  , loadedLossy :: !Bool
  -- ^ Whether the file was not UTF-8, so that bytes of it became U+FFFD and
  -- saving will not bring them back.
  }

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
    }

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

-- | Read a file as UTF-8, bytes that are not becoming U+FFFD, with its line
-- endings turned to @\\n@.
loadFile :: FilePath -> IO (Either Text Loaded)
loadFile path = do
  result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  pure $ case result of
    Left e -> Left (T.pack (show e))
    Right raw ->
      let (bom, bytes) = maybe (False, raw) ((,) True) (BS.stripPrefix utf8Bom raw)
          (lossy, text) = case TE.decodeUtf8' bytes of
            Right t -> (False, t)
            Left _ -> (True, TE.decodeUtf8With (\_ _ -> Just '\xFFFD') bytes)
          eol = if "\r\n" `T.isInfixOf` T.take 65536 text then CRLF else LF
          unix = if eol == CRLF then T.replace "\r\n" "\n" text else text
       in Right (Loaded (B.fromText unix) (FileFormat eol bom) lossy)

utf8Bom :: BS.ByteString
utf8Bom = "\xEF\xBB\xBF"

-- | Write the rope out a chunk at a time, never as one text, in the format
-- the file came in.
saveFile :: FilePath -> FileFormat -> Buffer -> IO (Either Text ())
saveFile path format buf = do
  result <- try write :: IO (Either SomeException ())
  pure (either (Left . T.pack . show) Right result)
  where
    write = do
      -- Before the file is opened, and with that emptied.
      rope <- evaluate (B.bufRope buf)
      withBinaryFile path WriteMode $ \h -> do
        when (formatBom format) (BS.hPut h utf8Bom)
        case formatEol format of
          LF -> Rope.hPutUtf8 h rope
          CRLF -> forM_ (Rope.toChunks rope) (BS.hPut h . TE.encodeUtf8 . T.replace "\n" "\r\n")

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
          }
  if not exists
    then pure (fresh B.empty (FileFormat LF False) ("New file " <> T.pack path))
    else
      loadFile path >>= \case
        Left err -> pure app {appStatus = "Could not open " <> T.pack path <> ": " <> err}
        Right loaded ->
          pure . fresh (loadedBuffer loaded) (loadedFormat loaded) $
            if loadedLossy loaded
              then "Opened " <> T.pack path <> ", which is not UTF-8: its other bytes are shown as " <> T.singleton (toEnum 0xFFFD) <> " and saving will not bring them back"
              else "Opened " <> T.pack path

--------------------------------------------------------------------------------
-- Window title
--------------------------------------------------------------------------------

foreign import ccall unsafe "SDL_SetWindowTitle"
  sdlSetWindowTitle :: Ptr () -> CString -> IO CBool

-- | The file's name, starred while it has changes to save.
titleFor :: App -> Text
titleFor app =
  maybe "Untitled" (T.pack . takeFileName) (appPath app)
    <> (if B.isDirty (edBuffer (appEditor app)) then " *" else "")
    <> " - ned"

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Run the editor, on a file if one is given.
runNed :: Maybe FilePath -> IO ()
runNed mpath = do
  app0 <- maybe (pure newApp) (`openPath` newApp) mpath
  ref <- newIORef app0
  -- NED_TRACE names a file to log a line a frame to: the time, the window's
  -- size, and what the frame cost.
  trace <- lookupEnv "NED_TRACE"
  -- NED_BLANK swaps the application for one label, to tell what a frame
  -- costs the toolkit from what it costs the editor.
  blank <- lookupEnv "NED_BLANK"
  let body = maybe (appView ref) (const (label "blank")) blank
      view = case trace of
        Nothing -> body
        Just file -> do
          inp <- askInput
          t0 <- uiIO getMonotonicTime
          body
          t1 <- uiIO getMonotonicTime
          let Size w h = inputWindowSize inp
          uiIO (appendFile file (printf "%.4f %.0f %.0f %.3f\n" t0 w h ((t1 - t0) * 1000)))
  runSdlApp
    defaultSdlOptions
      { sdlWindowTitle = titleFor app0
      , sdlWindowSize = Size 1100 760
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      }
    view

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

-- | What of the application the chrome draws, and what the editor draws. The
-- state is in an 'IORef' that nano-ui knows nothing of, so a frame that
-- changed it after the part showing it was declared (a menu button opening
-- its menu, a menu row editing the text, the find field setting what is
-- marked) has to ask for the frame that shows it.
chromeSig :: App -> (Text, Bool, Bool, Bool, Text)
chromeSig a = (appOpenMenu a, appBar a == BarNone, appBarFocus a, isJust (appPending a), appStatus a)

editorSig :: App -> (Int, Int, Int, Text, (Bool, Bool, Bool), Float, Text)
editorSig a =
  ( B.bufVersion buf
  , B.bufCursor buf
  , B.bufAnchor buf
  , edFind ed
  , (edFindExact ed, edReveal ed, edShowWhitespace ed)
  , edFontSize ed
  , langName (edLang ed)
  )
  where
    ed = appEditor a
    buf = edBuffer ed

appView :: IORef App -> NanoUI ()
appView ref = do
  ctx <- askContext
  inp <- askInput
  app0 <- uiIO (readIORef ref)
  drawn <- uiIO (newIORef (editorSig app0))
  let modify = uiIO . modifyIORef' ref
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
              , appStatus = "Saved " <> T.pack path
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
      zoom f = onEditor (\ed -> ed {edFontSize = max 8 (min 48 (f (edFontSize ed)))})

  ------------------------------------------------------------ file dialogs ---
  for_ (appOpenDlg app0) $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected paths -> do
        modify (\a -> a {appOpenDlg = Nothing})
        for_ (listToMaybe paths) (run . PendingOpenPath)
      _ -> modify (\a -> a {appOpenDlg = Nothing})
  for_ (appSaveDlg app0) $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected paths -> do
        modify (\a -> a {appSaveDlg = Nothing})
        for_ (listToMaybe paths) saveTo
      _ -> modify (\a -> a {appSaveDlg = Nothing})

  -- A file dropped on the window opens.
  for_ [T.unpack (dropEventData d) | d <- foldr (:) [] (inputDrops inp), dropEventType d == DropFile] $
    guarded . PendingOpenPath

  ----------------------------------------------------------------- chords ---
  let mods = inputModifiers inp
      blocked = isJust (appPending app0)
  when (modCtrl mods && not (modAlt mods) && not blocked) $
    forM_ (T.unpack (inputChars inp)) $ \case
      's' | modShift mods -> save True
      's' -> save False
      'S' -> save True
      'o' -> guarded PendingOpen
      'n' -> guarded PendingNew
      'q' -> guarded PendingQuit
      'f' -> openBar BarFind
      'g' -> openBar BarGoto
      '=' -> zoom (* 1.1)
      '+' -> zoom (* 1.1)
      '-' -> zoom (/ 1.1)
      '0' -> zoom (const defaultFontSize)
      _ -> pure ()
  when (inputKeysElem KeyEscape (inputKeys inp) && appBar app0 /= BarNone && not blocked) closeBar

  ------------------------------------------------------------------ menus ---
  let item lbl shortcut action =
        whenM (if T.null shortcut then menuItem lbl else menuItemShortcut lbl shortcut) (closeMenu >> action)
      itemIf ok lbl shortcut action = if ok then item lbl shortcut action else menuItemDisabled lbl
      buf0 = edBuffer (appEditor app0)
      fileMenu = do
        item "New" "Ctrl+N" (guarded PendingNew)
        item "Open..." "Ctrl+O" (guarded PendingOpen)
        item "Save" "Ctrl+S" (save False)
        item "Save As..." "Ctrl+Shift+S" (save True)
        menuSeparator
        item "Exit" "Ctrl+Q" (guarded PendingQuit)
      editMenu = do
        itemIf (B.canUndo buf0) "Undo" "Ctrl+Z" (onBuffer B.undo)
        itemIf (B.canRedo buf0) "Redo" "Ctrl+Y" (onBuffer B.redo)
        menuSeparator
        item "Cut" "Ctrl+X" (onBufferIO clipboardCut)
        item "Copy" "Ctrl+C" (onBufferIO clipboardCopy)
        item "Paste" "Ctrl+V" (onBufferIO clipboardPaste)
        menuSeparator
        item "Select All" "Ctrl+A" (onBuffer B.selectAll)
        menuSeparator
        item "Find..." "Ctrl+F" (openBar BarFind)
        item "Go to Line..." "Ctrl+G" (openBar BarGoto)
      viewMenu = do
        item "Zoom In" "Ctrl+=" (zoom (* 1.1))
        item "Zoom Out" "Ctrl+-" (zoom (/ 1.1))
        item "Reset Zoom" "Ctrl+0" (zoom (const defaultFontSize))
        menuSeparator
        item
          (if edShowWhitespace (appEditor app0) then "Hide Indentation Marks" else "Show Indentation Marks")
          ""
          (onEditor (\e -> e {edShowWhitespace = not (edShowWhitespace e)}))
        item
          (if B.usesTabs buf0 then "Indent with Spaces" else "Indent with Tabs")
          ""
          (onBuffer (B.setUsesTabs (not (B.usesTabs buf0))))
        item
          (if formatEol (appFormat app0) == LF then "Line Endings: CRLF" else "Line Endings: LF")
          ""
          (modify (\a -> a {appFormat = (appFormat a) {formatEol = if formatEol (appFormat a) == LF then CRLF else LF}}))

  ----------------------------------------------------------------- layout ---
  columnWith (grow . gap 0 . padAll 0) $ do
    menuBar
      (appOpenMenu app0)
      (\m -> modify (\a -> a {appOpenMenu = m}))
      (appMenuSwallow app0)
      (\m -> modify (\a -> a {appMenuSwallow = m}))
      [("File", fileMenu), ("Edit", editMenu), ("View", viewMenu)]
    separator

    -- The editor runs on the state as the chords and menus above left it.
    app1 <- uiIO (readIORef ref)
    let wantFocus = not (appBarFocus app1) && not blocked && T.null (appOpenMenu app1)
    (edResp, ed) <- editorView wantFocus (appEditor app1)
    modify (\a -> a {appEditor = ed, appBarFocus = appBarFocus a && not (edPressed ed)})
    app2 <- uiIO (readIORef ref)
    uiIO (writeIORef drawn (editorSig app2))

    _ <- contextMenu edResp $ do
      let buf = edBuffer ed
          pick lbl shortcut ok action =
            if ok then whenM (menuItemShortcut lbl shortcut) action else menuItemDisabled lbl
      pick "Undo" "Ctrl+Z" (B.canUndo buf) (onBuffer B.undo)
      pick "Redo" "Ctrl+Y" (B.canRedo buf) (onBuffer B.redo)
      menuSeparator
      pick "Cut" "Ctrl+X" True (onBufferIO clipboardCut)
      pick "Copy" "Ctrl+C" True (onBufferIO clipboardCopy)
      pick "Paste" "Ctrl+V" True (onBufferIO clipboardPaste)
      menuSeparator
      pick "Select All" "Ctrl+A" True (onBuffer B.selectAll)
      pick "Find..." "Ctrl+F" True (openBar BarFind)

    let enter = inputKeysElem KeyEnter (inputKeys inp) && appBarFocus app2
        -- Keep the keyboard in the bar's field while the bar has it.
        holdFocus resp =
          when (appBarFocus app2) $ uiIO $ do
            focus <- getFocusId ctx
            when (focus /= respId resp) $ writeIORef (ctxFocusId ctx) (respId resp)
    case appBar app2 of
      BarNone -> pure ()
      BarFind -> do
        separator
        rowWith (tight . fillW . gap 8 . padXY 8 4 . alignMid) $ do
          labelWith (tight . fontMuted) "Find"
          (resp, query) <- textInput' (appFindText app2)
          holdFocus resp
          when (respPressed resp) (modify (\a -> a {appBarFocus = True}))
          when (query /= appFindText app2) $ do
            modify (\a -> a {appFindText = query, appEditor = (appEditor a) {edFind = query}})
            -- Search as the query is typed, from where the selection starts.
            onBuffer (\b -> B.setCursor False (fst (B.selectionRange b)) b)
            find True
          exact <- checkbox "Match case" (edFindExact (appEditor app2))
          when (exact /= edFindExact (appEditor app2)) (onEditor (\e -> e {edFindExact = exact}))
          whenM (buttonWith tight "Previous") (find False)
          whenM (buttonWith tight "Next") (find True)
          whenM (buttonWith tight "Close") closeBar
          when enter (find (not (modShift mods)))
      BarGoto -> do
        separator
        rowWith (tight . fillW . gap 8 . padXY 8 4 . alignMid) $ do
          labelWith (tight . fontMuted) "Go to line"
          (resp, txt) <- textInput' (appGotoText app2)
          holdFocus resp
          when (txt /= appGotoText app2) (modify (\a -> a {appGotoText = txt}))
          go <- buttonWith tight "Go"
          whenM (buttonWith tight "Close") closeBar
          when (enter || go) $
            case readMaybe (T.unpack (T.strip txt)) of
              Just n -> onBuffer (B.gotoLine n) >> closeBar
              Nothing -> status "Not a line number"

    separator
    app3 <- uiIO (readIORef ref)
    statusBar app3

  --------------------------------------------------------------- overlays ---
  app4 <- uiIO (readIORef ref)
  when (titleFor app4 /= appTitle app4) $ do
    modify (\a -> a {appTitle = titleFor app4})
    host <- askHost
    for_ host $ \env ->
      uiIO (BS.useAsCString (TE.encodeUtf8 (titleFor app4)) (sdlSetWindowTitle (castPtr (sdlWindow env))))
  (_, _) <-
    modal (isJust (appPending app4)) "Unsaved changes" $ do
      label "This file has changes that are not saved."
      labelWith fontMuted "Discard them and carry on?"
      rowWith (fillW . gap 8) $ do
        flex
        whenM (button "Cancel") (modify (\a -> a {appPending = Nothing}))
        whenM (button "Discard") $ do
          modify (\a -> a {appPending = Nothing})
          for_ (appPending app4) run

  appEnd <- uiIO (readIORef ref)
  drawnSig <- uiIO (readIORef drawn)
  when (chromeSig appEnd /= chromeSig app0 || editorSig appEnd /= drawnSig) (uiIO (markDirty ctx))

-- | A horizontal menu bar. @open@ is the label of the open drop-down, or
-- empty. A click on a menu's button toggles the menu.
--
-- A popup is dismissed by the press of a click outside it, and a button is
-- clicked by the release. A click on the open menu's own button is both: the
-- press closes the menu, and the release would open it again. So a press
-- that closes a menu over its own button is remembered in @swallow@, and the
-- click that follows it does nothing.
menuBar :: Text -> (Text -> NanoUI ()) -> Text -> (Text -> NanoUI ()) -> [(Text, NanoUI ())] -> NanoUI ()
menuBar open setOpen swallow setSwallow entries = do
  -- The pointer as it is, where a button under an open menu sees none.
  pointer <- askFrameInput
  rowWith (tight . fillW . fixedH 28) $ do
    for_ entries $ \(title, body) -> do
      let isOpen = open == title
      btn <- menuButton' title isOpen
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
          onButton = rectContains (respRect btn) (inputMousePos pointer)
      when (respClicked btn && swallow /= title) (setOpen (if isOpen then "" else title))
      when (not isOpen && not (T.null open) && respHovered btn) (setOpen title)
      (popupResp, _) <- popup isOpen cfg (columnWith (tight . gap 0) body)
      when (respClicked popupResp) $ do
        setOpen ""
        when (inputMousePressed pointer && onButton) (setSwallow title)
    flex
  when (inputMouseReleased pointer && not (T.null swallow)) (setSwallow "")

statusBar :: App -> NanoUI ()
statusBar app =
  rowWith (tight . gap 16 . fillW . padXY 8 4) $ do
    labelWith (tight . fontMuted) (appStatus app)
    flex
    labelWith (tight . fontMuted) (maybe "Untitled" (T.pack . takeFileName) (appPath app) <> (if B.isDirty buf then " *" else ""))
    labelWith (tight . fontMuted) ("Ln " <> showT (ln + 1) <> ", Col " <> showT (col + 1))
    labelWith (tight . fontMuted) (showT (B.lineCount buf) <> " lines")
    labelWith (tight . fontMuted) (langName (edLang ed))
    labelWith (tight . fontMuted) (if B.usesTabs buf then "Tabs" else "Spaces")
    labelWith (tight . fontMuted) (T.pack (show (formatEol (appFormat app))) <> (if formatBom (appFormat app) then " BOM" else ""))
    labelWith (tight . fontMuted) (showT (round (edFontSize ed / defaultFontSize * 100) :: Int) <> "%")
  where
    ed = appEditor app
    buf = edBuffer ed
    (ln, col) = B.cursorPosition buf
    showT :: Show a => a -> Text
    showT = T.pack . show
