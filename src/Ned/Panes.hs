-- | The row the tree and the editor share: the two panes of a nano-ui pane
-- grid, so the bar between them is the toolkit's to draw, drag and remember.
--
-- The grid keeps its split tree in the widget store, keyed by its own widget
-- id, and seeds itself with a single pane when it finds none. A grid that had
-- to split itself would put the new pane on the B side and at a half, which
-- is the wrong side for the editor and the wrong width for the tree, so both
-- panes are seeded before its first frame, along with the row's rect, which
-- the first split is laid out from: the tree is the left pane, the editor the
-- right, and the tree starts at the width it always has.
module Ned.Panes
  ( -- * The row
    treeEditorGrid
  ) where

import Control.Monad (when)
import Data.Dynamic (toDyn)
import qualified Data.IntMap.Strict as IM
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import NanoUI
import NanoUI.Context
  ( Context (..)
  , DamageState (..)
  , Slot (..)
  , WidgetStore (..)
  , getStore
  , intKey
  , modifyDamage
  , setStore
  , slotKey
  )
import NanoUI.Monad (askContext)
import NanoUI.Widgets.SplitPane (GridNode (Pane), treeSetRatio, treeSplit)
import Ned.FileTree (defaultTreeWidth, minTreeWidth)
import Ned.Theme (paneChrome)
import Ned.Widget (dropFocus)

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
treeEditorGrid ::
  Ui :> es =>
  Eff es PaneView ->
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
        , pgViewPane = \pid pctx -> if pid == treePaneId then treePane else editorPane pctx
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
