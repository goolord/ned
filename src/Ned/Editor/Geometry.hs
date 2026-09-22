-- | Where the editor puts things: the cell a character sits in, the width of
-- the gutter, how much of the document the view holds.
--
-- The editor lays text out on a grid of cells rather than measuring each
-- glyph, which is what lets it draw a line as one op and still know where the
-- caret goes. Everything that turns a line and a column into pixels, or a
-- pixel back into a line, goes through here.
module Ned.Editor.Geometry
  ( -- * The grid
    Geometry (..)
  , geometry
  , cellWidth

    -- * The view
  , scrollBarW
  , scrollBarH
  , textPad
  , textWidth
  , viewLinesOf
  , maxScrollY
  , maxScrollXOf
  , scroller
  , hscroller
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Widget (Scroller (..))

scrollBarW, scrollBarH, textPad :: Float
scrollBarW = 12
-- | The sideways bar is as tall along the bottom as the upright one is wide.
scrollBarH = scrollBarW
textPad = 8

-- | What a frame needs to place text: the cell, the line, and where the text
-- starts within the widget.
data Geometry = Geometry
  { gCellW :: !Float
  , gLineH :: !Float
  , gGutterW :: !Float
  }

geometry :: Float -> FontMetrics -> Buffer -> Geometry
geometry cellW fm buf =
  let digits = max 3 (length (show (B.lineCount buf)))
   in Geometry
        { gCellW = cellW
        , gLineH = max 1 (fromIntegral (ceiling (fmLineHeight fm) :: Int))
        , gGutterW = fromIntegral (digits + 2) * cellW
        }

-- | The width of a cell: what a character of a run advances the pen by.
-- 'fmAdvance' is not that under a host that shapes. SDL_ttf gives a glyph's
-- advance in whole pixels and lays a shaped line out by the unrounded one, 8
-- and 8.25 at size 15, so cells a rounded advance wide part from the glyphs of
-- a run drawn as one op, by a cell every 32 characters. The width of a long
-- run over its length is the advance to within a pixel across a line.
--
-- It takes what measures a line in the font, which a view has as
-- 'lineWidthUi' and anything outside one as the backend's own.
cellWidth :: Monad m => (Text -> m Float) -> m Float
cellWidth measure = do
  w <- measure cellRuler
  pure (max 1 (w / fromIntegral (T.length cellRuler)))

-- | Short enough for the host to keep its shaped line from frame to frame.
cellRuler :: Text
cellRuler = T.replicate 256 "M"

-- | The width the text has to itself.
textWidth :: Geometry -> Rect -> Float
textWidth g r = max 1 (rectW r - gGutterW g - textPad - scrollBarW)

viewLinesOf :: Geometry -> Rect -> Double
viewLinesOf g r = realToFrac (rectH r / gLineH g)

maxScrollY :: Geometry -> Rect -> Buffer -> Double
maxScrollY g r buf = max 0 (fromIntegral (B.lineCount buf) - viewLinesOf g r + 1)

-- | The view and its scrollbar. The last line scrolls up to the top of the
-- view, so what there is to scroll through is the lines and a view more.
scroller :: Geometry -> Rect -> Buffer -> Scroller
scroller g r buf =
  let viewL = viewLinesOf g r
   in Scroller (rectH r) (viewL / (fromIntegral (B.lineCount buf) + viewL)) (maxScrollY g r buf)

-- | The view and its scrollbar, along the line: the lane is the rect's whole
-- width, and what there is to scroll through is the text's width beyond the
-- view's, over a line so many cells wide -- the widest on screen and the
-- caret's, as the view's own bound has it. A cell of the thumb travels a cell
-- of the view, until the thumb's least size has its say.
hscroller :: Geometry -> Rect -> Int -> Scroller
hscroller g r widestCells =
  let tw = textWidth g r
      travel = maxScrollXOf g r widestCells
   in Scroller (rectW r) (realToFrac ((rectW r - travel) / rectW r)) (realToFrac travel)

-- | How far the view travels sideways, in pixels, over a line so many cells
-- wide: the line's width past what the text has to itself.
maxScrollXOf :: Geometry -> Rect -> Int -> Float
maxScrollXOf g r widestCells =
  max 0 (fromIntegral widestCells * gCellW g - textWidth g r)
