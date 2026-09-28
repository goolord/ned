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
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import NanoUI
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Complete (Candidate (..), Completion (..), Source, menuLimit)
import Ned.Editor
import Ned.Editor.Geometry
import Ned.Highlight
import Ned.Text (cellOfCol, cellsAt, clamp, foldCase, indentOf)
import Ned.Theme
import Ned.View.Code
import Ned.Widget (markOp, rounding, thumbSpan)

--------------------------------------------------------------------------------
-- The editor
--------------------------------------------------------------------------------

-- | The editor, filling the space its parent gives it: four custom widgets,
-- the line numbers, the text and the upright scrollbar in a row, and the
-- sideways scrollbar in a lane under them. Pass the editor and keep the
-- result; the response is for hanging a context menu on, and the rectangle
-- is the caret's, for putting something by it. It takes the
-- keyboard when @wantFocus@ is set, which an application clears while a
-- field of its own is being typed into, marks the matches of @marks@, and
-- underlines @diagnostics@, each from and to an offset, by its severity.
-- @others@ offers words to complete from besides the text's own.
--
-- @textKey@ names the text the editor holds, and changes when it is another
-- text: two files just opened are at the same version with the caret and the
-- view in the same place, and without it the second would not be drawn.
editorView :: Int -> Bool -> Text -> [(Int, Int, Int)] -> Source -> Editor -> NanoUI (Response, Editor, Rect)
editorView textKey wantFocus marks diagnostics others ed0 = do
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

  fr <- editorFrame wantFocus others rect cellW fm ed0
  let ed1 = efEditor fr
      g = efGeometry fr
  -- The window hands over typed text only while a widget asks for it, and
  -- puts the input method's candidates by the caret it is given.
  _ <- useInputMethod wid InputNormal (caretRect g rect ed1)
  let scene =
        EditorScene
          { esKey = textKey
          , esBuffer = edBuffer ed1
          , esLang = edLang ed1
          , esLexStart = efLexStart fr
          , esScrollY = edScrollY ed1
          , esScrollX = edScrollX ed1
          , esFontSize = edFontSize ed1
          , esGeometry = g
          , esCaretOn = efCaretOn fr
          , esFind = if edFindExact ed1 then marks else foldCase marks
          , esFindExact = edFindExact ed1
          , esDiagnostics = diagnostics
          , esThumbHot = efThumbHot fr
          , esThumbXHot = efThumbXHot fr
          , esWhitespace = edShowWhitespace ed1
          , esBlock = efBlock fr
          , esMenu = edCompletion ed1
          }
      part which cursor layout =
        defaultCustomWidgetSpec
          { widgetLayout = layout defaultLayout
          , widgetDraw = \_ r -> drawEditor which scene r
          , widgetContent = editorSceneKey which scene
          , widgetCursor = Just (\_ _ _ -> cursor)
          , widgetDamageSlop = 0
          }
  resp <- columnWith (grow . gap 0 . padAll 0) $ do
    respRow <- rowWith (grow . gap 0 . padAll 0) $ do
      (respGutter, ()) <- customWidgetWithId widGutter (part PartGutter UiCursorDefault (fillH . fixedW (gGutterW g)))
      -- The text takes the keys a text area does, the editing chords among
      -- them, so a menu row bound to Ctrl+Z leaves that press to the text.
      (respText, ()) <- customWidgetWithId wid (part PartText UiCursorText grow) {widgetFocusable = True, widgetKeys = KeysType}
      _ <- customWidgetWithId widBar (part PartBar UiCursorDefault (fillH . fixedW scrollBarW))
      pure (respGutter <> respText)
    (respHBar, ()) <- customWidgetWithId widHBar (part PartHBar UiCursorDefault (fillW . fixedH scrollBarH))
    pure (respRow <> respHBar)
  pure (resp, ed1, caretRect g rect ed1)

-- | Everything the editor's drawing reads.
data EditorScene = EditorScene
  { esKey :: !Int
  -- ^ Which text it is ('editorView').
  , esBuffer :: !Buffer
  , esLang :: !Lang
  , esLexStart :: !LexState
  , esScrollY :: !Double
  , esScrollX :: !Float
  , esFontSize :: !Float
  , esGeometry :: !Geometry
  , esCaretOn :: !Bool
  , esFind :: !Text
  , esFindExact :: !Bool
  , esDiagnostics :: ![(Int, Int, Int)]
  , esThumbHot :: !Bool
  , esThumbXHot :: !Bool
  , esWhitespace :: !Bool
  , esBlock :: !(Maybe Int)
  -- ^ The character a block caret is on, in vim's modes that draw one.
  , esMenu :: !(Maybe Completion)
  }

-- | Where the caret is in the window, given the whole editor's rectangle, as
-- the text draws it.
caretRect :: Geometry -> Rect -> Editor -> Rect
caretRect g (Rect x y _ _) ed =
  Rect (textX + fromIntegral (B.colToVisual buf ln col) * gCellW g) (y + realToFrac (fromIntegral ln - edScrollY ed) * gLineH g) 2 (gLineH g)
  where
    buf = edBuffer ed
    (ln, col) = B.cursorPosition buf
    textX = x + gGutterW g + textPad - edScrollX ed

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
        PartHBar -> [keyPart (4 :: Int), keyPart (esScrollX sc), keyPart (esFontSize sc), keyPart (B.widestLine buf), keyPart (esThumbXHot sc)]
        PartText ->
          [ keyPart (3 :: Int)
          , keyPart (esKey sc)
          , keyPart (B.bufVersion buf)
          , keyPart (B.bufCursor buf)
          , keyPart (B.bufAnchor buf)
          , keyPart (esScrollY sc)
          , keyPart (esScrollX sc)
          , keyPart (esFontSize sc)
          , keyPart (esCaretOn sc)
          , keyPart (esWhitespace sc)
          , keyPart (fromMaybe (-1) (esBlock sc))
          , keyPart (esFindExact sc)
          , keyPart (esFind sc)
          , keyPart (show (esDiagnostics sc))
          , keyPart (langName (esLang sc))
          , keyPart (show (esLexStart sc))
          , -- The menu's words change with the text, which the version has
            -- already; what is left is which of them is picked.
            keyPart (maybe (-1) cmStart (esMenu sc))
          , keyPart (maybe (-1) cmPicked (esMenu sc))
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
            , concatMap underlines rows
            , caret
            , maybe [] menuOps (esMenu sc)
            ]
      PartBar ->
        FillRect own colBackground
          : FillRect own colTrack
          : [ FillRoundedRect (Rect (ox + 3) (y + thumbTop + 2) (scrollBarW - 6) (thumbH - 4)) rounding (if esThumbHot sc then colThumbHot else colThumb)
            | maxScrollY g rect buf > 0
            ]
      PartHBar ->
        FillRect own colBackground
          : FillRect own colTrack
          : [ FillRoundedRect (Rect (x + thumbLeft + 2) (oy + 3) (thumbW - 4) (scrollBarH - 6)) rounding (if esThumbXHot sc then colThumbHot else colThumb)
            | maxScrollX g rect buf > 0
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
    font = codeFont (esFontSize sc) TokPlain
    textX = x + gGutterW g + textPad - esScrollX sc
    firstLine = floor (esScrollY sc) :: Int
    yOff = realToFrac (fromIntegral firstLine - esScrollY sc) * lineH
    lastLine = min (B.lineCount buf - 1) (firstLine + ceiling (h / lineH))
    lineY ln = y + yOff + fromIntegral (ln - firstLine) * lineH
    -- The cells on screen, with one to spare on either side.
    firstCell = max 0 (floor (esScrollX sc / cellW) - 1) :: Int
    lastCell = firstCell + ceiling (w / cellW) + 2
    cellX c = textX + fromIntegral c * cellW
    textGrid = Grid textX cellW (esFontSize sc) firstCell lastCell
    (thumbTop, thumbH) = thumbSpan (scroller g rect buf) (esScrollY sc)
    (selFrom, selTo) = B.selectionRange buf
    (caretLine, caretCol) = B.cursorPosition buf

    -- The sideways bar: the lane is the editor's whole width, and the thumb
    -- stands for the share of the line the view holds.
    hbar = hscroller g rect buf
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

    band color ln = cellBand textGrid (lineY ln) lineH color

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

    -- What a language server found wrong on a row: a line under it along
    -- the foot of the row, over the text.
    underlines vr =
      [ FillRect (Rect (cellX c0) (lineY ln + lineH - 2) (fromIntegral (max 1 (c1 - c0)) * cellW) 2) (colDiagnostic sev)
      | let ln = rowLine vr
            start = B.lineStart buf ln
            end = start + B.lineLength buf ln
      , (i, j, sev) <- esDiagnostics sc
      , i <= end && j > start
      , let c0 = cellIn vr (max 0 (i - start))
            c1 = cellIn vr (min (end - start) (j - start))
      ]

    texts = concatMap lineOps rows
    lineOps vr
      | rowLong vr =
          [DrawTextStyled (cellX firstCell) (lineY (rowLine vr)) font (T.map visible (rowText vr)) (tokenColor TokPlain) | not (T.null (rowText vr))]
      | otherwise = codeOps textGrid (lineY (rowLine vr)) (rowText vr) (rowSpans vr)

    visible c = if c < ' ' then ' ' else c

    -- The band on the caret's line carries on through the gutter, so that the
    -- number and the text it belongs to read as one row rather than as a
    -- highlight that starts where the code does.
    gutterCurrent =
      [ FillRect (Rect ox (lineY caretLine) ow lineH) colCurrentLine
      | selFrom == selTo
      , caretLine >= firstLine
      , caretLine <= lastLine
      ]

    numbers = [lineNumber textGrid (x + gGutterW g) (lineY ln) (ln == caretLine) ln | ln <- [firstLine .. lastLine]]

    -- A bar before the caret's character, or with vim's keys a block over
    -- it, the character drawn again on it in the colour of the page.
    (blockLine, blockCol) = case esBlock sc of
      Nothing -> (caretLine, caretCol)
      Just off -> let ln = B.lineOf buf off in (ln, off - B.lineStart buf ln)
    caret =
      concat
        [ case esBlock sc of
            Nothing -> [FillRect (Rect (cellX cell) (lineY blockLine) 2 lineH) colCaret]
            Just _ ->
              FillRect (Rect (cellX cell) (lineY blockLine) (fromIntegral (max 1 (cellIn vr (blockCol + 1) - cell)) * cellW) lineH) colCaret
                : [ DrawTextStyled (cellX cell) (lineY blockLine) font (T.singleton ch) colBackground
                  | not (rowLong vr)
                  , blockCol < T.length (rowText vr)
                  , let ch = T.index (rowText vr) blockCol
                  , ch > ' '
                  ]
        | esCaretOn sc
        , vr <- rows
        , rowLine vr == blockLine
        , let cell = cellIn vr blockCol
        , cell >= firstCell
        , cell <= lastCell
        ]

    -- The completion menu, under the word it completes, or over it when
    -- there is no room below: a row a word, with where each is from in a
    -- column after it, and the one in the text picked out. The words stand
    -- on the cells of the word in the text, and the menu is kept to the text.
    menuOps m =
      FillRect menuRect colMenu
        : StrokeRoundedRect menuRect 0 1 colMenuEdge
        : concat (zipWith menuRow [0 ..] [firstRow .. firstRow + showing - 1])
        ++ menuThumb
      where
        shown = cmShown m
        total = V.length shown
        showing = min menuLimit total
        firstRow = max 0 (cmPicked m - showing + 1)
        widest f cap = min cap (V.maximum (V.map (\c -> let t = f c in cellOfCol t (T.length t)) shown))
        wordCells = widest candWord 48
        noteCells = widest candNote 24
        panelW = fromIntegral (1 + wordCells + (if noteCells > 0 then 2 + noteCells else 0) + 1) * cellW + 2
        panelH = fromIntegral showing * lineH + 2
        sLine = B.lineOf buf (cmStart m)
        sCell = B.colToVisual buf sLine (cmStart m - B.lineStart buf sLine)
        px = clamp ox (max ox (ox + ow - panelW)) (cellX sCell - cellW - 1)
        under = lineY sLine + lineH
        py = if under + panelH > oy + oh && lineY sLine - panelH >= oy then lineY sLine - panelH else under
        menuRect = Rect px py panelW panelH
        rowY k = py + 1 + fromIntegral (k :: Int) * lineH
        menuRow k i =
          let c = shown V.! i
              picked = i == cmPicked m
           in [FillRect (Rect (px + 1) (rowY k) (panelW - 2) lineH) colMenuPicked | picked]
                ++ [markOp (Rect (px + 1) (rowY k) 0 lineH) colCaret | picked]
                ++ [DrawTextStyled (px + 1 + cellW) (rowY k) font (T.take wordCells (candWord c)) (tokenColor TokPlain)]
                ++ [ DrawTextStyled (px + 1 + fromIntegral (wordCells + 3) * cellW) (rowY k) font (T.take noteCells (candNote c)) colGutterText
                   | not (T.null (candNote c))
                   ]
        -- A lane down the right edge while there are more words than show.
        menuThumb
          | total <= showing = []
          | otherwise =
              let trackH = panelH - 4
                  lane = max 6 (trackH * fromIntegral showing / fromIntegral total)
                  top = py + 2 + (trackH - lane) * fromIntegral firstRow / fromIntegral (total - showing)
               in [FillRect (Rect (px + panelW - 4) top 2 lane) colThumb]

-- | A line on screen, as the drawing has it: its number, whether it is one of
-- the long ones that are never read whole, the text it shows, and what the
-- lexer made of that.
data ViewRow = ViewRow
  { rowLine :: !Int
  , rowLong :: !Bool
  , rowText :: !Text
  , rowSpans :: ![Span]
  }
