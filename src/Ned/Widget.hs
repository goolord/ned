-- | What the editor and the file tree have in common. Both are custom nano-ui
-- widgets that scroll themselves a row at a time under a scrollbar of their
-- own, and both draw their icons out of the toolkit's shapes.
module Ned.Widget
  ( -- * Scrolling
    Scroller (..)
  , thumbSpan
  , thumbGrab
  , thumbScroll
  , followRow

    -- * Icons
  , folderIcon
  , fileIcon
  ) where

import NanoUI (Color, DrawOp (..), Rect (..))
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
-- Both are centred on @cy@, in a column @treeIcon@ wide starting at @ix@.
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
