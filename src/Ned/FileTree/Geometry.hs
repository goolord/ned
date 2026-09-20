-- | Where the file tree puts things: how tall a row is, how far it indents,
-- and the lane its scrollbar has.
--
-- The panel in "Ned.FileTree" hit-tests against some of these and the drawing
-- in "Ned.View" places everything by them, so they are written down once and
-- both read them. What colour any of it is is "Ned.Theme"'s to say: the tree
-- is chrome, so its greys are worked out from whatever theme is running
-- rather than written down here.
module Ned.FileTree.Geometry
  ( -- * The row
    treePad
  , treeIndent
  , treeChevron
  , treeIcon
  , treeMark
  , treeRadius
  , rowHeight

    -- * The panel
  , treeBarW
  , treeHeaderPad
  , treeHeaderHeight

    -- * The view
  , maxScroll
  , scroller
  ) where

import NanoUI
import Ned.Widget (Scroller (..))

--------------------------------------------------------------------------------
-- The row
--------------------------------------------------------------------------------

treePad, treeIndent, treeChevron, treeIcon, treeBarW, treeMark :: Float
treePad = 8
treeIndent = 14
treeChevron = 14

-- | The column an icon sits in, between the chevron and the name. A file has
-- no chevron but still leaves room for one, so that the icons of a folder and
-- of the files inside it line up in a column.
treeIcon = 18
treeBarW = 10

-- | The bar down the left of the row whose file the editor has open. It is its
-- own mark and not the selection's, because arrowing through the tree moves
-- the selection away and the open file has to stay findable.
treeMark = 3

-- | The only corner in the window, and the scrollbar thumb's own. It goes on
-- the things a pointer grabs or picks and on nothing else; the caret, the
-- selection bands and the rules are all square.
treeRadius :: Float
treeRadius = 3

-- | A row is scanned rather than read, so it sits tighter than a line of text.
rowHeight :: FontMetrics -> Float
rowHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 2

--------------------------------------------------------------------------------
-- The panel
--------------------------------------------------------------------------------

-- | Where the panel's header sets the root's name: the column a row's icon
-- starts in, so that the name of the folder lines up with what is under it
-- rather than sitting against the panel's edge.
treeHeaderPad :: Float
treeHeaderPad = treePad + treeChevron + 2

-- | The panel's header: the root's name, with the header's own padding above
-- and below it. The pane grid's drag picks the pane by this strip.
treeHeaderHeight :: FontMetrics -> Float
treeHeaderHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 12

--------------------------------------------------------------------------------
-- The view
--------------------------------------------------------------------------------

maxScroll :: Int -> Double -> Double
maxScroll rowCount viewRows = max 0 (fromIntegral rowCount - viewRows)

-- | The view and its scrollbar, over so many rows.
scroller :: Rect -> Int -> Double -> Scroller
scroller rect rowCount viewRows =
  Scroller (rectH rect) (viewRows / max 1 (fromIntegral rowCount)) (maxScroll rowCount viewRows)
