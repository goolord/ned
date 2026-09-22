-- | The editor's four widgets, and the draw ops they build.
--
-- The line numbers, the text and the two scrollbars are four custom nano-ui
-- widgets rather than one: the toolkit runs a frame for a pointer that only
-- moved when it came over another widget, and takes the cursor's shape from
-- the widget under it, so this is what changes the cursor the moment it
-- crosses onto a scrollbar. The text's widget is the one with the keyboard.
--
-- Nothing here reads input or keeps state. What a frame of the editor works
-- out is "Ned.Editor"'s, and what it hands back, together with the editor
-- itself, is gathered into an 'EditorScene' -- everything the drawing looks at
-- and nothing else. 'editorSceneKey' is a number over the same values, so a
-- frame in which none of them changed builds no ops and repaints nothing.
module Ned.View.Editor
  ( editorView
  ) where

import Control.Monad (when)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
import NanoUI
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor
import Ned.Editor.Geometry
import Ned.Highlight
import Ned.Text (cellOfCol, cellsAt, foldCase, indentOf)
import Ned.Theme
import Ned.Widget (thumbSpan)

--------------------------------------------------------------------------------
-- The editor
--------------------------------------------------------------------------------

-- | The editor, filling the space its parent gives it: four custom widgets,
-- the line numbers, the text and the upright scrollbar in a row, and the
-- sideways scrollbar in a lane under them. Pass the editor and keep the
-- result; the response is for hanging a context menu on. It takes the
-- keyboard when @wantFocus@ is set, which an application clears while a
-- field of its own is being typed into.
--
-- nano-ui runs a frame for a pointer that only moved when it came over another
-- widget, and takes the cursor's shape from the widget under it, so four
-- widgets rather than one is what changes the cursor the moment it crosses
-- onto a scrollbar. The text's widget is the one with the keyboard.
editorView :: Ui :> es => Bool -> Editor -> Eff es (Response, Editor)
editorView wantFocus ed0 = do
  widGutter <- nextId
  wid <- nextId
  widBar <- nextId
  widHBar <- nextId
  fm <- resolveFontUi (edFontSize ed0) WeightNormal FontStyleNormal FontMono
  cellW <- cellWidth (lineWidthUi fm)
  -- The whole editor, from where its parts were last frame: the row's left
  -- and right, and the lane's bottom under it.
  prevGutter <- lastRect widGutter
  prevBar <- lastRect widBar
  prevHBar <- lastRect widHBar
  let rect = case (prevGutter, prevBar, prevHBar) of
        (Just (Rect gx gy _ _), Just (Rect bx _ bw _), Just (Rect _ hy _ hh)) ->
          Rect gx gy (bx + bw - gx) (hy + hh - gy)
        _ -> Rect 0 0 800 600

  -- Tab would walk the focus off to the menu bar, and a click on a menu takes
  -- it there; the editor takes it back for as long as it is wanted.
  when wantFocus (holdFocus wid)

  fr <- editorFrame wantFocus rect cellW fm ed0
  let ed1 = efEditor fr
      g = efGeometry fr
      scene =
        EditorScene
          { esBuffer = edBuffer ed1
          , esLang = edLang ed1
          , esLexStart = efLexStart fr
          , esScrollY = edScrollY ed1
          , esScrollX = edScrollX ed1
          , esFontSize = edFontSize ed1
          , esGeometry = g
          , esCaretOn = efCaretOn fr
          , esFind = if edFindExact ed1 then edFind ed1 else foldCase (edFind ed1)
          , esFindExact = edFindExact ed1
          , esThumbHot = efThumbHot fr
          , esThumbXHot = efThumbXHot fr
          , esWidest = efWidest fr
          , esWhitespace = edShowWhitespace ed1
          }
      part which pointer layout =
        defaultCustomWidgetSpec
          { widgetLayout = layout defaultLayout
          , widgetDraw = \_ r -> drawEditor which scene r
          , widgetContent = editorSceneKey which scene
          , widgetCursor = Just (const pointer)
          , widgetDamageSlop = 0
          }
  resp <- columnWith (grow . gap 0 . padAll 0) $ do
    respRow <- rowWith (grow . gap 0 . padAll 0) $ do
      (respGutter, ()) <- customWidgetWithId widGutter (part PartGutter UiCursorDefault (fillH . fixedW (gGutterW g)))
      (respText, ()) <- customWidgetWithId wid (part PartText UiCursorText grow) {widgetFocusable = True}
      _ <- customWidgetWithId widBar (part PartBar UiCursorDefault (fillH . fixedW scrollBarW))
      pure (respGutter <> respText)
    (respHBar, ()) <- customWidgetWithId widHBar (part PartHBar UiCursorDefault (fillW . fixedH scrollBarH))
    pure (respRow <> respHBar)
  pure (resp, ed1)

-- | Everything the editor's drawing reads.
data EditorScene = EditorScene
  { esBuffer :: !Buffer
  , esLang :: !Lang
  , esLexStart :: !LexState
  , esScrollY :: !Double
  , esScrollX :: !Float
  , esFontSize :: !Float
  , esGeometry :: !Geometry
  , esCaretOn :: !Bool
  , esFind :: !Text
  , esFindExact :: !Bool
  , esThumbHot :: !Bool
  , esThumbXHot :: !Bool
  , esWidest :: !Int
  -- ^ The widest the view scrolls sideways to, in cells, as the frame left it.
  , esWhitespace :: !Bool
  }

-- | The widgets the editor is made of.
data Part = PartGutter | PartText | PartBar | PartHBar

-- | A number that changes when what a part draws does. The version stands
-- for the text, and the language's name for its rules. The line numbers and
-- the two scrollbars read little of the scene, and are left alone by a caret
-- that blinks or moves along its line.
editorSceneKey :: Part -> EditorScene -> Int
editorSceneKey which sc =
  let buf = esBuffer sc
   in contentKeyOf $ case which of
        PartGutter -> [keyPart (1 :: Int), keyPart (B.lineCount buf), keyPart (esScrollY sc), keyPart (esFontSize sc), keyPart (fst (B.cursorPosition buf))]
        PartBar -> [keyPart (2 :: Int), keyPart (B.lineCount buf), keyPart (esScrollY sc), keyPart (esFontSize sc), keyPart (esThumbHot sc)]
        PartHBar -> [keyPart (4 :: Int), keyPart (esScrollX sc), keyPart (esFontSize sc), keyPart (esWidest sc), keyPart (esThumbXHot sc)]
        PartText ->
          [ keyPart (3 :: Int)
          , keyPart (B.bufVersion buf)
          , keyPart (B.bufCursor buf)
          , keyPart (B.bufAnchor buf)
          , keyPart (esScrollY sc)
          , keyPart (esScrollX sc)
          , keyPart (esFontSize sc)
          , keyPart (esCaretOn sc)
          , keyPart (esWhitespace sc)
          , keyPart (esFindExact sc)
          , keyPart (esFind sc)
          , keyPart (langName (esLang sc))
          , keyPart (show (esLexStart sc))
          ]

-- | The draw ops of one part, given the rectangle that part was laid out in.
-- They are worked out in terms of the whole editor, which starts a gutter to
-- the left of the text, ends an upright scrollbar to its right, and runs a
-- sideways scrollbar along its foot; each part is clipped to its own
-- rectangle.
drawEditor :: Part -> EditorScene -> Rect -> SmallArray DrawOp
drawEditor which sc own@(Rect ox oy ow oh) =
  smallArrayFromList $
    case which of
      PartGutter -> FillRect own colGutter : gutterCurrent ++ numbers
      PartText ->
        FillRect own colBackground
          : concat
            [ backdrops
            , if esWhitespace sc then concatMap indentation rows else []
            , texts
            , caret
            ]
      PartBar ->
        FillRect own colBackground
          : FillRect own colTrack
          : [ FillRoundedRect (Rect (ox + 3) (y + thumbTop + 2) (scrollBarW - 6) (thumbH - 4)) 3 (if esThumbHot sc then colThumbHot else colThumb)
            | maxScrollY g rect buf > 0
            ]
      PartHBar ->
        FillRect own colBackground
          : FillRect own colTrack
          : [ FillRoundedRect (Rect (x + thumbLeft + 2) (oy + 3) (thumbW - 4) (scrollBarH - 6)) 3 (if esThumbXHot sc then colThumbHot else colThumb)
            | maxScrollXOf g rect (esWidest sc) > 0
            ]
  where
    -- The whole editor. Nothing a part draws reads a side of it that the
    -- part's own rectangle does not give.
    rect@(Rect x y w h) = case which of
      PartGutter -> Rect ox oy (ow + scrollBarW) oh
      PartText -> Rect (ox - gGutterW g) oy (ow + gGutterW g + scrollBarW) oh
      PartBar -> Rect (ox + ow - scrollBarW) oy scrollBarW oh
      -- The sideways bar is a whole of its own: its lane is the editor's
      -- width, which its own rectangle already spans.
      PartHBar -> own
    buf = esBuffer sc
    g = esGeometry sc
    cellW = gCellW g
    lineH = gLineH g
    font = fontOf TokPlain
    fontOf kind = TextFont (esFontSize sc) FontMono (tokenWeight kind) FontStyleNormal DecorationNone
    textX = x + gGutterW g + textPad - esScrollX sc
    firstLine = floor (esScrollY sc) :: Int
    yOff = realToFrac (fromIntegral firstLine - esScrollY sc) * lineH
    lastLine = min (B.lineCount buf - 1) (firstLine + ceiling (h / lineH))
    lineY ln = y + yOff + fromIntegral (ln - firstLine) * lineH
    -- The cells on screen, with one to spare on either side.
    firstCell = max 0 (floor (esScrollX sc / cellW) - 1) :: Int
    lastCell = firstCell + ceiling (w / cellW) + 2
    cellX c = textX + fromIntegral c * cellW
    (thumbTop, thumbH) = thumbSpan (scroller g rect buf) (esScrollY sc)
    (selFrom, selTo) = B.selectionRange buf
    (caretLine, caretCol) = B.cursorPosition buf

    -- The sideways bar: the lane is the editor's whole width, and the thumb
    -- stands for the share of the line the view holds.
    hbar = hscroller g rect (esWidest sc)
    (thumbLeft, thumbW) = thumbSpan hbar (realToFrac (esScrollX sc))

    -- Each line on screen with its text: the whole of an ordinary line, and
    -- of a long one the columns on screen.
    rows = lexed (esLexStart sc) [firstLine .. lastLine]
    lexed _ [] = []
    lexed st (ln : rest)
      | B.isLongLine buf ln =
          let t = B.lineWindow buf ln firstCell lastCell
           in ViewRow ln True t [Span (T.length t) TokPlain] : lexed st rest
      | otherwise =
          let t = B.lineText buf ln
              (spans, st') = lexLine (esLang sc) st t
           in ViewRow ln False t spans : lexed st' rest

    -- The cell of a column of a row's line.
    cellIn vr col
      | rowLong vr = col
      | otherwise = cellOfCol (rowText vr) col

    clampCells c0 c1 = (max firstCell c0, min lastCell c1)
    band color ln c0 c1 =
      let (a, b) = clampCells c0 c1
       in [FillRect (Rect (cellX a) (lineY ln) (fromIntegral (b - a) * cellW) lineH) color | b > a]

    backdrops = concatMap backdrop rows
    backdrop vr =
      let ln = rowLine vr
          start = B.lineStart buf ln
          len = B.lineLength buf ln
          current = [FillRect (Rect x (lineY ln) w lineH) colCurrentLine | ln == caretLine && selFrom == selTo]
          selection
            | selFrom == selTo || selTo <= start || selFrom > start + len = []
            | otherwise =
                let c0 = cellIn vr (max 0 (selFrom - start))
                    c1 = cellIn vr (min len (selTo - start))
                    -- A selected newline shows as one more cell.
                    c1' = if selTo > start + len then c1 + 1 else c1
                 in band colSelection ln c0 c1'
       in current ++ matches vr ++ selection

    matches vr
      | T.null (esFind sc) = []
      | otherwise =
          let t = if esFindExact sc then rowText vr else foldCase (rowText vr)
              base = if rowLong vr then firstCell else 0
              n = T.length (esFind sc)
              go !col rest = case T.breakOn (esFind sc) rest of
                (_, m) | T.null m -> []
                (pre, m) ->
                  let c = col + T.length pre
                   in band colFindMatch (rowLine vr) (cellIn vr (base + c)) (cellIn vr (base + c + n))
                        ++ go (c + n) (T.drop n m)
           in go 0 t

    -- The indentation of a line: a dot in the middle of each space, and a
    -- rule along each tab.
    indentation vr
      | rowLong vr = []
      | otherwise = marks 0 (T.unpack (indentOf (rowText vr)))
      where
        midY = lineY (rowLine vr) + fromIntegral (round (lineH / 2) :: Int)
        marks _ [] = []
        marks !cell (c : cs)
          | cell > lastCell = []
          | c == ' ' = [FillRect (Rect (cellX cell + cellW / 2 - 1) (midY - 1) 2 2) colWhitespace | cell >= firstCell] ++ marks (cell + 1) cs
          | otherwise =
              let next = cell + cellsAt cell '\t'
               in [FillRect (Rect (cellX cell + 2) midY (fromIntegral (next - cell) * cellW - 4) 1) colWhitespace | next > firstCell] ++ marks next cs

    texts = concatMap lineOps rows
    lineOps vr
      | rowLong vr =
          [DrawTextStyled (cellX firstCell) (lineY (rowLine vr)) font (T.map visible (rowText vr)) (tokenColor TokPlain) | not (T.null (rowText vr))]
      | otherwise = runs (lineY (rowLine vr)) 0 (rowText vr) (rowSpans vr)

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
      [ DrawTextStyled (x + gGutterW g - cellW - fromIntegral (T.length num) * cellW) (lineY ln) font num color
      | ln <- [firstLine .. lastLine]
      , let num = T.pack (show (ln + 1))
            color = if ln == caretLine then colGutterActive else colGutterText
      ]

    caret =
      [ FillRect (Rect (cellX cell) (lineY caretLine) 2 lineH) colCaret
      | esCaretOn sc
      , vr <- rows
      , rowLine vr == caretLine
      , let cell = cellIn vr caretCol
      , cell >= firstCell
      , cell <= lastCell
      ]

-- | A line on screen, as the drawing has it: its number, whether it is one of
-- the long ones that are never read whole, the text it shows, and what the
-- lexer made of that.
data ViewRow = ViewRow
  { rowLine :: !Int
  , rowLong :: !Bool
  , rowText :: !Text
  , rowSpans :: ![Span]
  }
