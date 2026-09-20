-- | The draw ops of one frame of the editor.
--
-- Nothing here reads input or keeps state. A 'Scene' is everything the
-- drawing looks at, gathered by "Ned.Editor" before it hands the three
-- widgets over to nano-ui; 'sceneKey' is a number over the same values, so a
-- frame in which none of them changed builds no ops and repaints nothing.
module Ned.Editor.Draw
  ( Scene (..)
  , Part (..)
  , sceneKey
  , drawScene
  ) where

import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import NanoUI hiding (label, row)
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor.Geometry
import Ned.Highlight
import Ned.Text (cellOfCol, cellsAt, foldCase, indentOf)
import Ned.Theme
import Ned.Widget (contentHash, hashText, thumbSpan)

-- | Everything the drawing reads.
data Scene = Scene
  { scBuffer :: !Buffer
  , scLang :: !Lang
  , scLexStart :: !LexState
  , scScrollY :: !Double
  , scScrollX :: !Float
  , scFontSize :: !Float
  , scGeometry :: !Geometry
  , scCaretOn :: !Bool
  , scFind :: !Text
  , scFindExact :: !Bool
  , scThumbHot :: !Bool
  , scWhitespace :: !Bool
  }

-- | A number that changes when what a part draws does. The version stands
-- for the text, and the language's name for its rules. The line numbers and
-- the scrollbar read little of the scene, and are left alone by a caret that
-- blinks or moves along its line.
sceneKey :: Part -> Scene -> Int
sceneKey which sc =
  let buf = scBuffer sc
      scrollY = fromIntegral (castDoubleToWord64 (scScrollY sc))
      sizeBits = fromIntegral (castFloatToWord32 (scFontSize sc))
      fields = case which of
        PartGutter -> [1, B.lineCount buf, scrollY, sizeBits, fst (B.cursorPosition buf)]
        PartBar -> [2, B.lineCount buf, scrollY, sizeBits, fromEnum (scThumbHot sc)]
        PartText ->
          [ 3
          , B.bufVersion buf
          , B.bufCursor buf
          , B.bufAnchor buf
          , scrollY
          , fromIntegral (castFloatToWord32 (scScrollX sc))
          , sizeBits
          , fromEnum (scCaretOn sc)
          , fromEnum (scWhitespace sc)
          , fromEnum (scFindExact sc)
          , hashText (scFind sc)
          , hashText (langName (scLang sc))
          , hashText (T.pack (show (scLexStart sc)))
          ]
   in contentHash fields

-- | The widgets the editor is made of.
data Part = PartGutter | PartText | PartBar

-- | The draw ops of one part, given the rectangle that part was laid out in.
-- They are worked out in terms of the whole editor, which starts a gutter to
-- the left of the text and ends a scrollbar to its right, and each part is
-- clipped to its own rectangle.
drawScene :: Part -> Scene -> Rect -> SmallArray DrawOp
drawScene which sc own@(Rect ox oy ow oh) =
  smallArrayFromList $
    case which of
      PartGutter -> FillRect own colGutter : gutterCurrent ++ numbers
      PartText ->
        FillRect own colBackground
          : concat
            [ backdrops
            , if scWhitespace sc then concatMap indentation rows else []
            , texts
            , caret
            ]
      PartBar ->
        FillRect own colBackground
          : FillRect own colTrack
          : [ FillRoundedRect (Rect (ox + 3) (y + thumbTop + 2) (scrollBarW - 6) (thumbH - 4)) 3 (if scThumbHot sc then colThumbHot else colThumb)
            | maxScrollY g rect buf > 0
            ]
  where
    -- The whole editor. Nothing a part draws reads a side of it that the
    -- part's own rectangle does not give.
    rect@(Rect x y w h) = case which of
      PartGutter -> Rect ox oy (ow + scrollBarW) oh
      PartText -> Rect (ox - gGutterW g) oy (ow + gGutterW g + scrollBarW) oh
      PartBar -> Rect (ox + ow - scrollBarW) oy scrollBarW oh
    buf = scBuffer sc
    g = scGeometry sc
    cellW = gCellW g
    lineH = gLineH g
    font = fontOf TokPlain
    fontOf kind = TextFont (scFontSize sc) FontMono (tokenWeight kind) FontStyleNormal DecorationNone
    textX = x + gGutterW g + textPad - scScrollX sc
    firstLine = floor (scScrollY sc) :: Int
    yOff = realToFrac (fromIntegral firstLine - scScrollY sc) * lineH
    lastLine = min (B.lineCount buf - 1) (firstLine + ceiling (h / lineH))
    lineY ln = y + yOff + fromIntegral (ln - firstLine) * lineH
    -- The cells on screen, with one to spare on either side.
    firstCell = max 0 (floor (scScrollX sc / cellW) - 1) :: Int
    lastCell = firstCell + ceiling (w / cellW) + 2
    cellX c = textX + fromIntegral c * cellW
    (thumbTop, thumbH) = thumbSpan (scroller g rect buf) (scScrollY sc)
    (selFrom, selTo) = B.selectionRange buf
    (caretLine, caretCol) = B.cursorPosition buf

    -- Each line on screen with its text: the whole of an ordinary line, and
    -- of a long one the columns on screen.
    rows = lexed (scLexStart sc) [firstLine .. lastLine]
    lexed _ [] = []
    lexed st (ln : rest)
      | B.isLongLine buf ln =
          let t = B.lineWindow buf ln firstCell lastCell
           in ViewRow ln True t [Span (T.length t) TokPlain] : lexed st rest
      | otherwise =
          let t = B.lineText buf ln
              (spans, st') = lexLine (scLang sc) st t
           in ViewRow ln False t spans : lexed st' rest

    -- The cell of a column of a row's line.
    cellIn row col
      | rowLong row = col
      | otherwise = cellOfCol (rowText row) col

    clampCells c0 c1 = (max firstCell c0, min lastCell c1)
    band color ln c0 c1 =
      let (a, b) = clampCells c0 c1
       in [FillRect (Rect (cellX a) (lineY ln) (fromIntegral (b - a) * cellW) lineH) color | b > a]

    backdrops = concatMap backdrop rows
    backdrop row =
      let ln = rowLine row
          start = B.lineStart buf ln
          len = B.lineLength buf ln
          current = [FillRect (Rect x (lineY ln) w lineH) colCurrentLine | ln == caretLine && selFrom == selTo]
          selection
            | selFrom == selTo || selTo <= start || selFrom > start + len = []
            | otherwise =
                let c0 = cellIn row (max 0 (selFrom - start))
                    c1 = cellIn row (min len (selTo - start))
                    -- A selected newline shows as one more cell.
                    c1' = if selTo > start + len then c1 + 1 else c1
                 in band colSelection ln c0 c1'
       in current ++ matches row ++ selection

    matches row
      | T.null (scFind sc) = []
      | otherwise =
          let t = if scFindExact sc then rowText row else foldCase (rowText row)
              base = if rowLong row then firstCell else 0
              n = T.length (scFind sc)
              go !col rest = case T.breakOn (scFind sc) rest of
                (_, m) | T.null m -> []
                (pre, m) ->
                  let c = col + T.length pre
                   in band colFindMatch (rowLine row) (cellIn row (base + c)) (cellIn row (base + c + n))
                        ++ go (c + n) (T.drop n m)
           in go 0 t

    -- The indentation of a line: a dot in the middle of each space, and a
    -- rule along each tab.
    indentation row
      | rowLong row = []
      | otherwise = marks 0 (T.unpack (indentOf (rowText row)))
      where
        midY = lineY (rowLine row) + fromIntegral (round (lineH / 2) :: Int)
        marks _ [] = []
        marks !cell (c : cs)
          | cell > lastCell = []
          | c == ' ' = [FillRect (Rect (cellX cell + cellW / 2 - 1) (midY - 1) 2 2) colWhitespace | cell >= firstCell] ++ marks (cell + 1) cs
          | otherwise =
              let next = cell + cellsAt cell '\t'
               in [FillRect (Rect (cellX cell + 2) midY (fromIntegral (next - cell) * cellW - 4) 1) colWhitespace | next > firstCell] ++ marks next cs

    texts = concatMap rowText' rows
    rowText' row
      | rowLong row =
          [DrawTextStyled (cellX firstCell) (lineY (rowLine row)) font (T.map visible (rowText row)) (tokenColor TokPlain) | not (T.null (rowText row))]
      | otherwise = runs (lineY (rowLine row)) 0 (rowText row) (rowSpans row)

    visible c = if c < ' ' then ' ' else c

    -- The draw ops of a line's spans, from a cell on. A run of plain ASCII is
    -- one op; anything else is placed a character at a time, so that the
    -- grid holds whatever a fallback font makes of it, or a heavier weight,
    -- whose glyphs advance further than a cell.
    runs _ _ _ [] = []
    runs ly cell t (Span n kind : rest)
      | cell > lastCell = []
      | otherwise =
          let seg = T.take n t
              t' = T.drop n t
           in if T.all simple seg && tokenWeight kind == WeightNormal
                then
                  let skip = max 0 (firstCell - cell)
                      keep = min n (lastCell - cell + 1) - skip
                      op = DrawTextStyled (cellX (cell + skip)) ly (fontOf kind) (T.take keep (T.drop skip seg)) (tokenColor kind)
                   in [op | keep > 0, not (T.all (== ' ') seg)] ++ runs ly (cell + n) t' rest
                else
                  let (ops, cell') = chars ly kind cell seg
                   in ops ++ runs ly cell' t' rest

    simple c = c >= ' ' && c < '\x7F'

    chars ly kind = go []
      where
        go acc !cell t = case T.uncons t of
          Nothing -> (reverse acc, cell)
          Just (c, r)
            -- A tab, a space and what is off screen take their cells and
            -- draw nothing.
            | c <= ' ' || cell < firstCell || cell > lastCell -> go acc next r
            | otherwise -> go (DrawTextStyled (cellX cell) ly (fontOf kind) (T.singleton c) (tokenColor kind) : acc) next r
            where
              next = cell + cellsAt cell c

    -- The band on the caret's line carries on through the gutter, so that the
    -- number and the text it belongs to read as one row rather than as a
    -- highlight that starts where the code does.
    gutterCurrent =
      [ FillRect (Rect ox (lineY caretLine) ow lineH) colCurrentLine
      | selFrom == selTo
      , caretLine >= firstLine
      , caretLine <= lastLine
      ]

    numbers =
      [ DrawTextStyled (x + gGutterW g - cellW - fromIntegral (T.length label) * cellW) (lineY ln) font label color
      | ln <- [firstLine .. lastLine]
      , let label = T.pack (show (ln + 1))
            color = if ln == caretLine then colGutterActive else colGutterText
      ]

    caret =
      [ FillRect (Rect (cellX cell) (lineY caretLine) 2 lineH) colCaret
      | scCaretOn sc
      , row <- rows
      , rowLine row == caretLine
      , let cell = cellIn row caretCol
      , cell >= firstCell
      , cell <= lastCell
      ]

data ViewRow = ViewRow
  { rowLine :: !Int
  , rowLong :: !Bool
  , rowText :: !Text
  , rowSpans :: ![Span]
  }
