-- | A line of code on the cell grid, as the editor draws one and the finder's
-- preview draws one the same way.
--
-- The grid is what lets a line be drawn in a few ops and still have every
-- character where the caret, the selection and a match expect it: a column
-- is a cell, and a cell is so many pixels, whatever the font would rather do.
module Ned.View.Code
  ( Grid (..)
  , codeFont
  , codeOps
  , cellBand
  , lineNumber
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import Ned.Highlight (Span (..), TokenKind (..))
import Ned.Text (cellsAt)
import Ned.Theme (colGutterActive, colGutterText, tokenColor, tokenWeight)

-- | Where a line's cells are, and which of them are in view.
data Grid = Grid
  { gridX :: !Float
  -- ^ Where the line's first cell starts.
  , gridCellW :: !Float
  , gridFontSize :: !Float
  , gridFirst :: !Int
  -- ^ The first cell in view, and the last: what is outside them is not drawn.
  , gridLast :: !Int
  }

cellX :: Grid -> Int -> Float
cellX g c = gridX g + fromIntegral c * gridCellW g

codeFont :: Float -> TokenKind -> TextFont
codeFont size kind = TextFont size FontMono (tokenWeight kind) FontStyleNormal DecorationNone

-- | The draw ops of a line's spans, with its top at @y@. A run of plain ASCII
-- in the normal weight is one op; anything else is placed a character at a
-- time, so that the grid holds whatever a fallback font makes of it, or a
-- heavier weight, whose glyphs advance further than a cell. A tab and a space
-- take their cells and draw nothing.
codeOps :: Grid -> Float -> Text -> [Span] -> [DrawOp]
codeOps g y = runs 0
  where
    runs _ _ [] = []
    runs !cell t (Span n kind : rest)
      | cell > gridLast g = []
      | T.all simple seg && tokenWeight kind == WeightNormal =
          let skip = max 0 (gridFirst g - cell)
              keep = min n (gridLast g - cell + 1) - skip
           in [DrawTextStyled (cellX g (cell + skip)) y (font kind) (T.take keep (T.drop skip seg)) (tokenColor kind) | keep > 0, not (T.all (== ' ') seg)]
                ++ runs (cell + n) t' rest
      | otherwise = chars kind cell (T.unpack seg) (\cell' -> runs cell' t' rest)
      where
        (seg, t') = T.splitAt n t
    chars _ !cell [] next = next cell
    chars kind !cell (c : cs) next =
      [DrawTextStyled (cellX g cell) y (font kind) (T.singleton c) (tokenColor kind) | c > ' ', cell >= gridFirst g, cell <= gridLast g]
        ++ chars kind (cell + cellsAt cell c) cs next
    simple c = c >= ' ' && c < '\x7F'
    font = codeFont (gridFontSize g)

-- | A band behind the cells of a line from one up to another, as much of it
-- as is in view: a selection, or a match.
cellBand :: Grid -> Float -> Float -> Color -> Int -> Int -> [DrawOp]
cellBand g y h color c0 c1 =
  let a = max (gridFirst g) c0
      b = min (gridLast g) c1
   in [FillRect (Rect (cellX g a) y (fromIntegral (b - a) * gridCellW g) h) color | b > a]

-- | A line's number, counted from one, set against a cell short of the
-- gutter's right edge at @right@: bright on the line the caret is on.
lineNumber :: Grid -> Float -> Float -> Bool -> Int -> DrawOp
lineNumber g right y current ln =
  DrawTextStyled (right - gridCellW g * fromIntegral (T.length num + 1)) y (codeFont (gridFontSize g) TokPlain) num $
    if current then colGutterActive else colGutterText
  where
    num = T.pack (show (ln + 1))
