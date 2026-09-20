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

    -- * Focus
  , takeFocus
  ) where

import Control.Monad (when)
import Data.Bits (xor)
import Data.IORef (writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI (WidgetId)
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
