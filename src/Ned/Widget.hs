-- | What the editor, the file tree and the finder have in common. They are
-- custom nano-ui widgets that lay themselves out a line or a row at a time;
-- the editor and the tree scroll themselves under a scrollbar of their own,
-- the tree and the finder draw rows of the same shape, and both of those
-- draw their icons out of the toolkit's shapes.
module Ned.Widget
  ( -- * Scrolling
    Scroller (..)
  , thumbSpan
  , thumbGrab
  , thumbScroll
  , followRow

    -- * Lines and rows
  , lineHeight
  , rowHeight
  , rowPad
  , rowMark
  , rowIcon
  , rounding
  , rowBand
  , markOp

    -- * Icons
  , folderIcon
  , fileIcon
  ) where

import NanoUI (Color, DrawOp (..), FontMetrics (..), Rect (..))
import Ned.Text (clamp)

--------------------------------------------------------------------------------
-- Scrolling
--------------------------------------------------------------------------------

-- | A view onto rows, and the scrollbar beside it.
data Scroller = Scroller
  { scrollTrack :: !Float
  -- ^ The height of the scrollbar's track, which is the view's.
  , scrollShown :: !Double
  -- ^ The share of what there is to scroll through that the view holds.
  , scrollRange :: !Double
  -- ^ How far the view scrolls, in rows.
  }

-- | The scrollbar's thumb with the view scrolled to a row: its top and its
-- height, within the track.
thumbSpan :: Scroller -> Double -> (Float, Float)
thumbSpan s at =
  let h = scrollTrack s
      thumbH = clamp 28 h (h * realToFrac (scrollShown s))
      frac = if scrollRange s <= 0 then 0 else realToFrac (at / scrollRange s)
   in ((h - thumbH) * frac, thumbH)

-- | How far below the thumb's top a press at @y@ takes hold of it: where it
-- landed, or the thumb's middle for a press on the track, which brings the
-- thumb to the pointer.
thumbGrab :: Scroller -> Double -> Float -> Float
thumbGrab s at y =
  let (top, thumbH) = thumbSpan s at
   in if y >= top && y <= top + thumbH then y - top else thumbH / 2

-- | The row to scroll to for a thumb held by @grab@ to follow a pointer at @y@.
thumbScroll :: Scroller -> Float -> Float -> Double
thumbScroll s grab y =
  let track = scrollTrack s - snd (thumbSpan s 0)
   in if track <= 0 then 0 else realToFrac ((y - grab) / track) * scrollRange s

-- | The scroll nearest @y@ that has a row inside a view of so many rows.
followRow :: Int -> Double -> Double -> Double
followRow row view y
  | r < y = r
  | r + 1 > y + view = r + 1 - max 1 (fromIntegral (floor view :: Int))
  | otherwise = y
  where
    r = fromIntegral row

--------------------------------------------------------------------------------
-- Lines and rows
--------------------------------------------------------------------------------

-- | A line of code: the font's line, to the whole pixel, so that lines stack
-- on the pixel grid.
lineHeight :: FontMetrics -> Float
lineHeight fm = max 1 (fromIntegral (ceiling (fmLineHeight fm) :: Int))

-- | A row of a list. A row is scanned rather than read, so it sits tighter
-- than a line of text would in a paragraph, and looser than a line of code.
rowHeight :: FontMetrics -> Float
rowHeight fm = lineHeight fm + 2

-- | The margin a row keeps from the side of its panel; the column its icon
-- sits in, before its name; and the width of the mark down its left.
--
-- The mark is its own and not the band's, because the band moves as the
-- keyboard walks the rows, and what the mark marks -- the file the editor has
-- open, the row Enter would open -- has to stay findable.
rowPad, rowIcon, rowMark :: Float
rowPad = 8
rowIcon = 18
rowMark = 3

-- | The only corner in the window. It goes on the things a pointer grabs or
-- picks -- a scrollbar's thumb, a row's band -- and on nothing else; the
-- caret, the selection and the rules are all square.
rounding :: Float
rounding = 3

-- | The band behind a row the keyboard is on or the pointer is over. It is
-- inset rather than run from edge to edge, so the panel keeps a margin down
-- both sides and the mark has somewhere of its own to sit.
rowBand :: Rect -> Color -> DrawOp
rowBand (Rect x y w h) = FillRoundedRect (Rect (x + rowMark + 2) (y + 1) (max 0 (w - rowMark - 4)) (h - 2)) rounding

-- | The mark down the left of a row.
markOp :: Rect -> Color -> DrawOp
markOp (Rect x y _ h) = FillRect (Rect x (y + 1) rowMark (h - 2))

--------------------------------------------------------------------------------
-- Icons
--------------------------------------------------------------------------------

-- | The icons, built out of the shapes the toolkit has rather than loaded
-- from an image: two of them, because two is all a list of files has to say.
-- A folder is a body under a tab. A file is a page with its top corner taken
-- off, which is the shape everyone reads as a document.
--
-- The corner is taken off rather than shaded over: the page is drawn as the
-- five-sided shape it ends up being, so what shows through the cut is
-- whatever the row behind it is, and one icon draws the same over a row that
-- is picked, hovered or plain. A shade would have to know the row's colour,
-- and at eleven pixels it did not read as a fold anyway.
--
-- Both are square-cornered. The one radius in the window belongs to the
-- things a pointer grabs or picks; an icon is neither.
--
-- Both are centred on @cy@, in a column 'rowIcon' wide starting at @ix@.
folderIcon :: Float -> Float -> Color -> [DrawOp]
folderIcon ix cy col =
  [ FillRect (Rect fx (cy - 5) 5 2) col
  , FillRect (Rect fx (cy - 3) 12 8) col
  ]
  where
    fx = fromIntegral (round ix :: Int) + 2

fileIcon :: Float -> Float -> Color -> [DrawOp]
fileIcon ix cy col =
  [ FillRect (Rect fx fy 5 4) col
  , FillRect (Rect fx (fy + 4) 9 8) col
  , FillTriangle (fx + 5) fy (fx + 9) (fy + 4) (fx + 5) (fy + 4) col
  ]
  where
    fx = fromIntegral (round ix :: Int) + 4
    fy = cy - 6
