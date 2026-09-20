-- | The application around the editor widget: what one frame of the whole
-- window does, and the entry point that runs them.
--
-- The pieces it puts together are all elsewhere -- the editor in
-- "Ned.Editor", the tree in "Ned.FileTree", files in "Ned.File", the bars in
-- "Ned.App.Chrome", the pane grid they sit in in "Ned.Panes", what they ask
-- for in "Ned.App.Commands", the state all of it runs on in "Ned.App.State" --
-- so what is left here is the order things happen in: dialogs, then chords,
-- then the menus, then the tree and the editor side by side, then the bar
-- under them, then the status bar, and the overlays over the lot.
module Ned.App
  ( -- * Running
    runNed
  , appView

    -- * The state it runs on
  , App (..)
  , newApp
  , newAppIn
  , openPath
  ) where

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Context (Context (..), markDirty)
import NanoUI.Monad (askContext, askHost, askInput)
import Ned.App.Chrome
import Ned.App.Commands
import Ned.App.State
import qualified Ned.Buffer as B
import Ned.Editor
import Ned.FileTree (rootName, fileTreePanel, ftPressed)
import Ned.FileTree.Draw (treeHeaderHeight)
import qualified Ned.FileTree as FT
import Ned.Highlight (langName)
import Ned.Panes (treeEditorGrid)
import Ned.Sdl (setWindowTitle)
import System.Environment (lookupEnv)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Run the editor, on a file if one is given.
runNed :: Maybe FilePath -> IO ()
runNed mpath = do
  blankApp <- newAppIn
  app0 <- maybe (pure blankApp) (`openPath` blankApp) mpath
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
-- A frame
--------------------------------------------------------------------------------

-- | What of the application the chrome draws, and what the editor draws. The
-- state is in an 'IORef' that nano-ui knows nothing of, so a frame that
-- changed it after the part showing it was declared (a menu button opening
-- its menu, a menu row editing the text, the find field setting what is
-- marked) has to ask for the frame that shows it.
--
-- The tree's width is not in here: the pane grid marks its own damage while
-- one of its bars is dragged.
chromeSig :: App -> (Text, Bool, Bool, Bool, Text, (Bool, Bool, Int))
chromeSig a =
  ( appOpenMenu a
  , appBar a == BarNone
  , appBarFocus a
  , isJust (appPending a)
  , appStatus a
  , (appTreeShown a, appTreeFocus a, FT.ftVersion (appTree a))
  )

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
  let cmds = commands ctx ref
      modify = cmdModify cmds
      guarded = cmdGuarded cmds
      run = cmdRun cmds

  ------------------------------------------------------------ file dialogs ---
  -- A dialog that is up is asked for its answer, and put away once it has one.
  let pollDialog dialog forget onPick =
        for_ (dialog app0) $ \did ->
          pollFileDialogUi did >>= \case
            FileDialogPending -> pure ()
            FileDialogSelected paths -> modify forget >> for_ (listToMaybe paths) onPick
            _ -> modify forget
  pollDialog appOpenDlg (\a -> a {appOpenDlg = Nothing}) (run . PendingOpenPath)
  pollDialog appSaveDlg (\a -> a {appSaveDlg = Nothing}) (cmdSaveTo cmds)

  -- A file dropped on the window opens.
  for_ [T.unpack (dropEventData d) | d <- foldr (:) [] (inputDrops inp), dropEventType d == DropFile] $
    guarded . PendingOpenPath

  ----------------------------------------------------------------- chords ---
  let mods = inputModifiers inp
      blocked = isJust (appPending app0)
  when (modCtrl mods && not (modAlt mods) && not blocked) $
    forM_ (T.unpack (inputChars inp)) $ \case
      's' | modShift mods -> cmdSave cmds True
      's' -> cmdSave cmds False
      'S' -> cmdSave cmds True
      'o' -> guarded PendingOpen
      'n' -> guarded PendingNew
      'q' -> guarded PendingQuit
      'f' -> cmdOpenBar cmds BarFind
      'g' -> cmdOpenBar cmds BarGoto
      'b' -> cmdToggleTree cmds
      '=' -> cmdZoom cmds (* 1.1)
      '+' -> cmdZoom cmds (* 1.1)
      '-' -> cmdZoom cmds (/ 1.1)
      '0' -> cmdZoom cmds (const defaultFontSize)
      _ -> pure ()
  when (inputKeysElem KeyEscape (inputKeys inp) && appBar app0 /= BarNone && not blocked) (cmdCloseBar cmds)

  ----------------------------------------------------------------- layout ---
  columnWith (grow . gap 0 . padAll 0) $ do
    menuBar
      (appOpenMenu app0)
      (\m -> modify (\a -> a {appOpenMenu = m}))
      (appMenuSwallow app0)
      (\m -> modify (\a -> a {appMenuSwallow = m}))
      (appMenus cmds app0)
    separator

    -- The tree and the editor run on the state as the chords and menus above
    -- left it. They are the two panes of a pane grid, so the bar between them
    -- is the toolkit's to draw and drag, and where the split was left is
    -- remembered by the grid rather than by the application.
    app1 <- uiIO (readIORef ref)
    let unblocked = not blocked && T.null (appOpenMenu app1)
        -- The tree pane: the panel, and what a frame's clicks on it left
        -- behind. The find bar's field takes the keyboard from the tree as it
        -- does from the editor, so the arrows do not walk both at once.
        treePane respRef pctx = do
          (resp, ft, opened) <-
            fileTreePanel
              (appTreeFocus app1 && not (appBarFocus app1) && unblocked)
              (appPath app1)
              (appTree app1)
          uiIO (writeIORef respRef (Just resp))
          modify $ \a ->
            a
              { appTree = ft
              , appTreeFocus = appTreeFocus a || ftPressed ft
              , appBarFocus = appBarFocus a && not (ftPressed ft)
              }
          -- A file the tree was clicked on opens as any other does, with the
          -- text asked about if it has changes to lose, and the keyboard
          -- going to it so that it can be typed into at once.
          for_ opened $ \path -> do
            modify (\a -> a {appTreeFocus = False})
            guarded (PendingOpenPath path)
          -- The pane grid moves the pane by the pick it is told about; the
          -- tree's is its header, the strip of the pane the root's name
          -- stands on, so a hold there drags the pane as the grid's own bars
          -- are dragged and nothing else does.
          hdrCtx <- askContext
          let (Rect px py pw _) = pgcRect pctx
              hh = treeHeaderHeight (ctxFontMetrics hdrCtx)
          pure (PaneView (rootName ft) False (Just (Rect px py pw hh)))
        -- The editor pane. Putting the tree away makes this pane the whole
        -- row: the grid calls that maximizing it, and keeps the split where
        -- it was, so showing the tree again brings it back at its width.
        editorPane respRef pctx = do
          if appTreeShown app1
            then when (pgcMaximized pctx) (pgcRestore pctx)
            else unless (pgcMaximized pctx) (pgcMaximize pctx)
          -- The tree may have just opened a file, which is the editor's
          -- buffer now. Who has the keyboard is read from before the tree
          -- ran, though: the keys of this frame are the tree's, and an Enter
          -- that opened a file there is not one to put a newline in the file
          -- it opened.
          appNow <- uiIO (readIORef ref)
          let wantFocus = not (appBarFocus app1) && not (appTreeFocus app1) && unblocked
          (resp, ed) <- editorView wantFocus (appEditor appNow)
          uiIO (writeIORef respRef (Just resp))
          modify $ \a ->
            a
              { appEditor = ed
              , appBarFocus = appBarFocus a && not (edPressed ed)
              , appTreeFocus = appTreeFocus a && not (edPressed ed)
              }
          pure (PaneView "" False Nothing)
    (mTreeResp, edResp) <- do
      treeRespRef <- uiIO (newIORef Nothing)
      edRespRef <- uiIO (newIORef Nothing)
      _ <-
        treeEditorGrid
          (treePane treeRespRef)
          (editorPane edRespRef)
      (,) <$> uiIO (readIORef treeRespRef) <*> uiIO (readIORef edRespRef)
    app2 <- uiIO (readIORef ref)
    uiIO (writeIORef drawn (editorSig app2))

    -- Scoped so that the editor's own menu below keeps its ids whether or not
    -- the tree hangs its own menu this frame.
    scope $ for_ mTreeResp $ \treeResp -> contextMenu treeResp (treeMenu cmds app2)

    for_ edResp $ \edMenuResp ->
      contextMenu edMenuResp (editorMenu cmds (edBuffer (appEditor app2)))

    editorBar cmds app2

    separator
    app3 <- uiIO (readIORef ref)
    statusBar app3

  --------------------------------------------------------------- overlays ---
  app4 <- uiIO (readIORef ref)
  when (titleFor app4 /= appTitle app4) $ do
    modify (\a -> a {appTitle = titleFor app4})
    host <- askHost
    for_ host $ \env -> uiIO (setWindowTitle env (titleFor app4))
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
