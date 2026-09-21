-- | What the editor and the file tree have in common. Both are custom nano-ui
-- widgets that scroll themselves a row at a time under a scrollbar of their
-- own, key their drawing on everything it reads, and keep the keyboard for as
-- long as it is theirs.
module Ned.Widget
  ( -- * Scrolling
    Scroller (..)
  , thumbSpan
  , thumbGrab
  , thumbScroll
  , followRow

    -- * Content keys
  , contentHash
  , hashText

    -- * Icons
  , folderIcon
  , fileIcon

    -- * Focus
  , takeFocus
  , dropFocus
  ) where

import Control.Monad (when)
import Data.Bits (xor)
import Data.IORef (writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI (Color, DrawOp (..), Rect (..), WidgetId (..))
import NanoUI.Context (Context (..), getFocusId)
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
-- Content keys
--------------------------------------------------------------------------------

-- | A custom widget's content key: a number that changes when any of the
-- values its drawing reads does (FNV-1a over them). Zero means "no key" to
-- nano-ui, so a hash that lands there becomes 1.
contentHash :: [Int] -> Int
contentHash fields = if h == 0 then 1 else h
  where
    h = foldl' fnv fnvBasis fields

-- | A text as one of the values of a 'contentHash'.
hashText :: Text -> Int
hashText = T.foldl' (\acc c -> fnv acc (fromEnum c)) fnvBasis

fnv :: Int -> Int -> Int
fnv acc v = (acc `xor` v) * 1099511628211

fnvBasis :: Int
fnvBasis = 1469598103934665603

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

--------------------------------------------------------------------------------
-- Focus
--------------------------------------------------------------------------------

-- | Give a widget the keyboard, without the ring that Tab would draw on it.
-- Tab walks the focus off to the next widget and a click on a menu takes it
-- there, so a widget that wants the keyboard takes it back every frame.
takeFocus :: Context -> WidgetId -> IO ()
takeFocus ctx wid = do
  focus <- getFocusId ctx
  when (focus /= wid) $ do
    writeIORef (ctxFocusId ctx) wid
    writeIORef (ctxFocusVisible ctx) False

-- | Take the keyboard off a widget that should not act on it. The pane grid
-- is focusable like any other nano-ui container, and its own keys act on its
-- panes; ned's widgets own the keyboard this side of it, so a focus left on
-- the grid by a Tab is dropped before the grid can read a key.
dropFocus :: Context -> WidgetId -> IO ()
dropFocus ctx wid = do
  focus <- getFocusId ctx
  when (focus == wid) (writeIORef (ctxFocusId ctx) (WidgetId 0))
