-- | One frame of the window, and the row its two panes share.
--
-- From the top: what the frame answers to, the menus along it, the tree and
-- the editor side by side in their pane grid, the find bar under them, the
-- status bar under that, and the overlays over the lot. What each of those is
-- made of is one module down -- the editor's widgets in "Ned.View.Editor", the
-- tree's in "Ned.View.Tree", the bars and menus in "Ned.View.Chrome" -- so what
-- is left here is the order they go in and the room each of them gets.
--
-- Nothing in this module or the three under it decides anything. What a key
-- does to the text is in "Ned.Editor.Keys", what a frame of the editor works
-- out from its input in "Ned.Editor", the tree's in "Ned.FileTree", what the
-- application can be asked to do in "Ned.App.Commands" and what it is between
-- frames in "Ned.App.State"; every button and menu row here asks for one of
-- those, so what a thing says and what it does sit on the same line.
module Ned.View
  ( appView
  , blankView
  , tracedView
  ) where

import Control.Monad (unless, when)
import Data.Dynamic (toDyn)
import Data.Foldable (for_)
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Context
  ( Context (..)
  , DamageState (..)
  , Slot (..)
  , WidgetStore (..)
  , getStore
  , intKey
  , markDirty
  , modifyDamage
  , setStore
  , slotKey
  )
import NanoUI.Monad (askContext, askInput)
import NanoUI.Widgets.SplitPane (GridNode (Pane), treeSetRatio, treeSplit)
import Ned.App.Commands
import Ned.App.Frame
import Ned.App.State
import Ned.Editor (Editor (..))
import Ned.FileTree (FileTree (..), defaultTreeWidth, minTreeWidth, rootName)
import qualified Ned.FileTree.Geometry as TG
import Ned.Theme (paneChrome)
import Ned.View.Chrome
import Ned.View.Editor (editorView)
import Ned.View.Tree (fileTreePanel)
import Ned.Widget (dropFocus)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- One frame of the window
--------------------------------------------------------------------------------

-- | The whole window, in the order things happen in: what the frame answers
-- to, then the menus, then the tree and the editor side by side, then the bar
-- under them, then the status bar, and the overlays over all of it.
--
-- The state is in an 'IORef' that nano-ui knows nothing of, so the frame reads
-- it again after each part that may have changed it, and asks for another
-- frame at the end when what it drew is no longer what the state says.
appView :: IORef App -> NanoUI ()
appView ref = do
  ctx <- askContext
  app0 <- uiIO (readIORef ref)
  drawn <- uiIO (newIORef (editorSig app0))
  let cmds = commands ctx ref
      modify = cmdModify cmds
      guarded = cmdGuarded cmds
      run = cmdRun cmds

  -- A dialog that is up is asked for its answer, a file dropped on the window
  -- opens, and the chords the application owns are read, all before anything
  -- is placed: what they leave behind is what the frame goes on to draw.
  pollDialogs cmds app0
  appChords cmds app0

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
    -- Whether the question about unsaved changes was up as the frame found
    -- things, which is what the chords above were read under too: one that
    -- puts the question up leaves this frame's keys where they were going.
    let blocked = isJust (appPending app0)
        unblocked = not blocked && T.null (appOpenMenu app1)
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
              hh = TG.treeHeaderHeight (ctxFontMetrics hdrCtx)
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
  syncTitle cmds app4
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

-- | One label and nothing else, which NED_BLANK swaps the application for: it
-- tells what a frame costs nano-ui from what it costs the editor.
blankView :: NanoUI ()
blankView = label "blank"

-- | A view with what it cost logged a line a frame to the named file: the
-- time, the window's size, and the milliseconds spent building it. NED_TRACE
-- asks for this.
tracedView :: FilePath -> NanoUI () -> NanoUI ()
tracedView file body = do
  inp <- askInput
  t0 <- uiIO getMonotonicTime
  body
  t1 <- uiIO getMonotonicTime
  let Size w h = inputWindowSize inp
  uiIO (appendFile file (printf "%.4f %.0f %.0f %.3f\n" t0 w h ((t1 - t0) * 1000)))

--------------------------------------------------------------------------------
-- The row the tree and the editor share
--------------------------------------------------------------------------------

-- The two panes of a nano-ui pane grid, so the bar between them is the
-- toolkit's to draw, drag and remember.
--
-- The grid keeps its split tree in the widget store, keyed by its own widget
-- id, and seeds itself with a single pane when it finds none. A grid that had
-- to split itself would put the new pane on the B side and at a half, which
-- is the wrong side for the editor and the wrong width for the tree, so both
-- panes are seeded before its first frame, along with the row's rect, which
-- the first split is laid out from: the tree is the left pane, the editor the
-- right, and the tree starts at the width it always has.

-- | The tree pane's id, the editor's, and the split between them. The ids
-- are the grid's to give out; these are the ones 'seedTreeEditor' wrote.
treePaneId, editorPaneId, treeEditorSplit :: Word64
treePaneId = 1
editorPaneId = 2
treeEditorSplit = 3

-- | The bar's measurements. The panes are laid out with a 'dividerW' gap
-- between them, of which the middle 'paneSpacing' is the line that is drawn;
-- the rest is grab margin, so the bar reads as the hairline the tree's old
-- splitter was without being hard to take hold of.
dividerW, paneSpacing, paneLeeway :: Float
dividerW = paneSpacing + 2 * paneLeeway
paneSpacing = 1
paneLeeway = 2

-- | The id after the seeded tree's: the first a pane made later may take, so
-- that no two panes ever share one.
treeEditorNext :: Word64
treeEditorNext = treeEditorSplit + 1

-- | The tree and the editor side by side, as the two panes of a pane grid.
-- Each pane's content is the given view, and the bar between them resizes
-- them; the split is kept by the grid, so the tree comes back at the width it
-- was left at when it is put away and taken up again.
--
-- The tree is the grid's pinned pane. A resized window resizes the editor:
-- the tree is a fixture the reader set the width of, and a wider window is
-- room for more text, not for more of a file name. It gives way only when
-- the window is too narrow to hold it and the editor's minimum both.
treeEditorGrid ::
  Ui :> es =>
  (PaneGridCtx es -> Eff es PaneView) ->
  (PaneGridCtx es -> Eff es PaneView) ->
  Eff es PaneGridResponse
treeEditorGrid treePane editorPane = do
  ctx <- askContext
  winW <- windowWidth
  winH <- windowHeight
  styled paneChrome $ do
    -- The grid's own id, which everything it keeps is keyed by. The seeding
    -- has to be in the store before the grid looks for it.
    wid <- currentId
    uiIO (seedTreeEditor ctx wid (Rect 0 0 winW winH) (treeShare winW))
    uiIO (dropFocus ctx wid)
    paneGrid
      defaultPaneGridConfig
        { pgSpacing = paneSpacing
        , pgMinSize = minTreeWidth
        , pgLeeway = paneLeeway
        , pgPreserveDragSize = True
        , -- The window's width is the editor's to take or give up: the tree
          -- is as wide as it was left, whatever the window does.
          pgFixedPanes = (== treePaneId)
        , pgViewPane = \pid pctx -> if pid == treePaneId then treePane pctx else editorPane pctx
        }
  where
    -- The tree's share of the row: the width the tree has always started at,
    -- of what the panes share out. The row spans the window and the grid has
    -- not been laid out yet, so the window's width stands in for the grid's.
    treeShare winW
      | usable <= 0 = 0.5
      | otherwise = defaultTreeWidth / usable
      where
        usable = winW - dividerW

-- | Put both panes in the store, and the row's rect in the damage state,
-- before the grid's first frame. The grid reads the tree it keeps under the
-- key of its own widget id, and lays a split out from the rect the widget had
-- last frame; on the first frame there is none, and the grid would lay the row
-- out at a half before the seeded share of it took effect. The rect is the
-- window's, since the row spans it and has not been laid out yet.
seedTreeEditor :: Context -> WidgetId -> Rect -> Float -> IO ()
seedTreeEditor ctx wid rect share = do
  st <- getStore ctx
  let k = intKey wid
  when (IM.notMember k (storeDyn st)) $ do
    setStore ctx $
      st
        { storeDyn = IM.insert k (toDyn panes) (storeDyn st)
        , -- Above every id in the tree, so a pane made by a gesture later
          -- cannot take an id a pane's own state is already under.
          storeInt = IM.insert (slotKey SlotPaneNext k) (fromIntegral treeEditorNext) (storeInt st)
        }
    modifyDamage ctx $ \ds -> ds {dsPrevRects = IM.insert k rect (dsPrevRects ds)}
  where
    panes =
      treeSetRatio
        treeEditorSplit
        share
        (treeSplit treePaneId treeEditorSplit AxisV False editorPaneId (Pane treePaneId))
