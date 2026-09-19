-- | The editor widget: three custom nano-ui widgets side by side (line
-- numbers, text, scrollbar) that draw the lines of the rope that are on
-- screen, and turn the frame's keys and pointer into edits.
--
-- It scrolls by itself, in lines, and asks the rope for the lines it shows and
-- no others, so a frame costs the same in a document of ten lines as in one of
-- ten million. Its drawing is keyed on everything it reads: a frame in which
-- none of that changed builds no draw ops and repaints nothing.
module Ned.View
  ( Editor (..)
  , newEditor
  , editorView
  , revealCaret
  , clipboardCopy
  , clipboardCut
  , clipboardPaste
  , defaultFontSize
  ) where

import Control.Monad (when)
import Data.Bits (shiftR, xor)
import Data.IORef (writeIORef)
import Data.Maybe (isNothing)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import NanoUI hiding (label, row)
import NanoUI.Context (Context (..), getFocusId, getPrevRect)
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Monad (askContext, askInput, uiTime)
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Highlight

data Drag
  = DragNone
  | -- | Selecting text.
    DragSelect
  | -- | Selecting by words, on from the word a double click took.
    DragWords !Int !Int
  | -- | Selecting whole lines from the gutter, starting at this line.
    DragLines !Int
  | -- | Holding the scrollbar's thumb, this far below its top.
    DragThumb !Float
  deriving (Eq)

data Editor = Editor
  { edBuffer :: !Buffer
  , edLang :: !Lang
  , edScrollY :: !Double
  -- ^ The line at the top of the view; its fraction is how far that line is
  -- scrolled out.
  , edScrollX :: !Float
  -- ^ Pixels.
  , edFontSize :: !Float
  , edDrag :: !Drag
  , edBlinkEpoch :: !Double
  -- ^ When the caret last moved: it shows steadily from then, and blinks after.
  , edLexCache :: !(Int, Int, LexState)
  -- ^ A version of the text, a line, and the lexer state that line starts in.
  , edFind :: !Text
  -- ^ What the view marks the matches of.
  , edFindExact :: !Bool
  , edReveal :: !Bool
  -- ^ Asks the next frame to scroll the caret into view.
  , edViewLines :: !Int
  -- ^ Whole lines that fit the view, as of the last frame.
  , edPressed :: !Bool
  -- ^ Whether the pointer went down on the editor this frame.
  , edShowWhitespace :: !Bool
  -- ^ Whether the indentation of a line is drawn: a dot a space, a rule a tab.
  }

defaultFontSize :: Float
defaultFontSize = 15

newEditor :: Lang -> Buffer -> Editor
newEditor lang buf =
  Editor
    { edBuffer = buf
    , edLang = lang
    , edScrollY = 0
    , edScrollX = 0
    , edFontSize = defaultFontSize
    , edDrag = DragNone
    , edBlinkEpoch = 0
    , edLexCache = (-1, 0, LexNormal)
    , edFind = T.empty
    , edFindExact = False
    , edReveal = True
    , edViewLines = 1
    , edPressed = False
    , edShowWhitespace = True
    }

-- | Have the next frame scroll the caret into view.
revealCaret :: Editor -> Editor
revealCaret ed = ed {edReveal = True}

--------------------------------------------------------------------------------
-- Look
--------------------------------------------------------------------------------

rgb :: Int -> Color
rgb v = colorRGBA (fromIntegral (v `shiftR` 16)) (fromIntegral (v `shiftR` 8)) (fromIntegral v) 255

colBackground, colGutter, colGutterText, colGutterActive, colCurrentLine, colSelection, colFindMatch, colCaret, colThumb, colThumbHot, colWhitespace :: Color
colBackground = rgb 0x1D1F21
colGutter = rgb 0x1D1F21
colGutterText = rgb 0x5A5E63
colGutterActive = rgb 0xC5C8C6
colCurrentLine = rgb 0x26292C
colSelection = rgb 0x3A4A5E
colFindMatch = rgb 0x5C4B1C
colCaret = rgb 0xE0E0E0
colThumb = rgb 0x3A3E44
colThumbHot = rgb 0x555A62
colWhitespace = rgb 0x3E4247

tokenColor :: TokenKind -> Color
tokenColor = \case
  TokPlain -> rgb 0xC5C8C6
  TokKeyword -> rgb 0xB294BB
  TokType -> rgb 0xF0C674
  TokFunction -> rgb 0x81A2BE
  TokString -> rgb 0xB5BD68
  TokNumber -> rgb 0xDE935F
  TokComment -> rgb 0x7C7F80
  TokPunct -> rgb 0x8ABEB7

scrollBarW, textPad :: Float
scrollBarW = 12
textPad = 8

-- | What a frame needs to place text: the cell, the line, and where the text
-- starts within the widget.
data Geometry = Geometry
  { gCellW :: !Float
  , gLineH :: !Float
  , gGutterW :: !Float
  }

geometry :: FontMetrics -> Buffer -> Geometry
geometry fm buf =
  let cellW = max 1 (fmAdvance fm 'M')
      digits = max 3 (length (show (B.lineCount buf)))
   in Geometry
        { gCellW = cellW
        , gLineH = max 1 (fromIntegral (ceiling (fmLineHeight fm) :: Int))
        , gGutterW = fromIntegral (digits + 2) * cellW
        }

-- | The width the text has to itself.
textWidth :: Geometry -> Rect -> Float
textWidth g r = max 1 (rectW r - gGutterW g - textPad - scrollBarW)

viewLinesOf :: Geometry -> Rect -> Double
viewLinesOf g r = realToFrac (rectH r / gLineH g)

maxScrollY :: Geometry -> Rect -> Buffer -> Double
maxScrollY g r buf = max 0 (fromIntegral (B.lineCount buf) - viewLinesOf g r + 1)

-- | The scrollbar's thumb: its top and its height, within the widget.
thumbSpan :: Geometry -> Rect -> Buffer -> Double -> (Float, Float)
thumbSpan g r buf scrollY =
  let h = rectH r
      total = fromIntegral (B.lineCount buf) + viewLinesOf g r
      thumbH = max 28 (min h (h * realToFrac (viewLinesOf g r / total)))
      range = maxScrollY g r buf
      frac = if range <= 0 then 0 else realToFrac (scrollY / range)
   in ((h - thumbH) * frac, thumbH)

--------------------------------------------------------------------------------
-- The widget
--------------------------------------------------------------------------------

-- | The editor, filling the space its parent gives it. Pass the editor and
-- keep the result; the response is for hanging a context menu on. It takes
-- the keyboard when @wantFocus@ is set, which an application clears while a
-- field of its own is being typed into.
editorView :: Ui :> es => Bool -> Editor -> Eff es (Response, Editor)
editorView wantFocus ed0 = do
  -- Three widgets side by side: the line numbers, the text and the scrollbar.
  -- nano-ui runs a frame for a pointer that only moved when it came over
  -- another widget, and takes the cursor's shape from the widget under it, so
  -- this is what changes the cursor the moment it crosses onto the scrollbar.
  -- The text's widget is the one with the keyboard.
  widGutter <- nextId
  wid <- nextId
  widBar <- nextId
  ctx <- askContext
  inp <- askInput
  now <- uiTime
  (fm, _) <- uiIO (ctxResolveFont ctx (edFontSize ed0) WeightNormal FontStyleNormal FontMono)
  -- The whole editor, from where its three parts were last frame.
  prevGutter <- uiIO (getPrevRect ctx widGutter)
  prevBar <- uiIO (getPrevRect ctx widBar)
  let rect = case (prevGutter, prevBar) of
        (Just (Rect gx gy _ gh), Just (Rect bx _ bw _)) -> Rect gx gy (bx + bw - gx) gh
        _ -> Rect 0 0 800 600

  -- Tab would walk the focus off to the menu bar, and a click on a menu takes
  -- it there; the editor takes it back for as long as it is wanted.
  focus0 <- uiIO (getFocusId ctx)
  when (wantFocus && focus0 /= wid) $ uiIO $ do
    writeIORef (ctxFocusId ctx) wid
    writeIORef (ctxFocusVisible ctx) False
  let focused = wantFocus

  let buf0 = edBuffer ed0
  buf1 <- if focused then applyKeys ctx inp (edViewLines ed0) buf0 else pure buf0

  let g = geometry fm buf1
      mouse = inputMousePos inp
      inside = rectContains rect mouse
      overBar = inside && v2X mouse >= rectX rect + rectW rect - scrollBarW
      overGutter = inside && v2X mouse < rectX rect + gGutterW g
      localY = v2Y mouse - rectY rect
      pointedLine scrollY b =
        max 0 (min (B.lineCount b - 1) (floor (scrollY + realToFrac (localY / gLineH g))))
      -- The offset under the pointer.
      pointed scrollY scrollX b =
        let ln = floor (scrollY + realToFrac (localY / gLineH g)) :: Int
            x = v2X mouse - (rectX rect + gGutterW g + textPad) + scrollX
         in B.offsetAt b ln (round (x / gCellW g))
      (thumbTop, thumbH) = thumbSpan g rect buf1 (edScrollY ed0)
      scrollToThumb grab =
        let range = maxScrollY g rect buf1
            track = rectH rect - thumbH
         in if track <= 0 then 0 else realToFrac ((localY - grab) / track) * range

  -- The pointer: a press starts a selection or takes the thumb, and a held
  -- button carries on with whichever it started.
  let (drag1, buf2, scrollY1)
        | inputMousePressed inp && overBar =
            let grab = if localY >= thumbTop && localY <= thumbTop + thumbH then localY - thumbTop else thumbH / 2
             in (DragThumb grab, buf1, scrollToThumb grab)
        | inputMousePressed inp && overGutter =
            -- A press on a line's number selects the line; with Shift, the
            -- lines from the selection's anchor to it.
            let ln = pointedLine (edScrollY ed0) buf1
                from = if modShift (inputModifiers inp) then B.lineOf buf1 (B.bufAnchor buf1) else ln
             in (DragLines from, B.selectLines from ln buf1, edScrollY ed0)
        | inputMousePressed inp && inside =
            -- The click count is the press's alone: the frames that hold the
            -- button report one click, so what the press took is kept in the
            -- drag, which goes on by words or by lines.
            let off = pointed (edScrollY ed0) (edScrollX ed0) buf1
             in case inputMouseClicks inp of
                  2 ->
                    let (i, j) = B.wordRangeAt off buf1
                     in (DragWords i j, B.selectWordAt off buf1, edScrollY ed0)
                  n
                    | n >= 3 ->
                        let ln = B.lineOf buf1 off
                         in (DragLines ln, B.selectLines ln ln buf1, edScrollY ed0)
                  _ -> (DragSelect, B.setCursor (modShift (inputModifiers inp)) off buf1, edScrollY ed0)
        | inputMouseRightPressed inp && inside && not overBar =
            -- A right press outside the selection moves the caret there, so
            -- that the menu it opens acts on what is under the pointer.
            let off = if overGutter then B.lineStart buf1 (pointedLine (edScrollY ed0) buf1) else pointed (edScrollY ed0) (edScrollX ed0) buf1
                (selFrom, selTo) = B.selectionRange buf1
                within = B.hasSelection buf1 && off >= selFrom && off <= selTo
             in (DragNone, if within then buf1 else B.setCursor False off buf1, edScrollY ed0)
        | not (inputMouseDown inp) = (DragNone, buf1, edScrollY ed0)
        | otherwise = case edDrag ed0 of
            DragThumb grab -> (DragThumb grab, buf1, scrollToThumb grab)
            DragSelect ->
              let sy = edgeScrolled
               in (DragSelect, B.setCursor True (pointed sy (edScrollX ed0) buf1) buf1, sy)
            DragWords i j ->
              let sy = edgeScrolled
               in (DragWords i j, B.selectWordsFrom (i, j) (pointed sy (edScrollX ed0) buf1) buf1, sy)
            DragLines from ->
              let sy = edgeScrolled
               in (DragLines from, B.selectLines from (pointedLine sy buf1) buf1, sy)
            DragNone -> (DragNone, buf1, edScrollY ed0)
      -- Past the top or bottom edge the view follows the pointer.
      edgeScrolled =
        let over
              | localY < 0 = realToFrac (localY / gLineH g)
              | localY > rectH rect = realToFrac ((localY - rectH rect) / gLineH g)
              | otherwise = 0
         in edScrollY ed0 + max (-3) (min 3 (over * 0.5))
      selecting = drag1 == DragSelect || isWords drag1 || isLines drag1
      autoScrolling = selecting && (localY < 0 || localY > rectH rect)
  when autoScrolling (wakeAfter 0.03)

  -- The wheel, three lines a notch; with Shift it scrolls sideways.
  let V2 wheelX wheelY = if inside then inputScroll inp else V2 0 0
      shift = modShift (inputModifiers inp)
      scrollY2 = scrollY1 + realToFrac (if shift then 0 else wheelY) * 3
      scrollX2 = edScrollX ed0 + (wheelX + (if shift then wheelY else 0)) * 3 * gCellW g

  -- Follow the caret when it moved, and then keep the scroll within bounds.
  let caretMoved =
        B.bufCursor buf2 /= B.bufCursor buf0
          || B.bufVersion buf2 /= B.bufVersion buf0
          || edReveal ed0
      (cLine, cCol) = B.cursorPosition buf2
      cCell = B.colToVisual buf2 cLine cCol
      viewL = viewLinesOf g rect
      followY y
        | not caretMoved || autoScrolling || isLines drag1 = y
        | fromIntegral cLine < y = fromIntegral cLine
        | fromIntegral cLine + 1 > y + viewL = fromIntegral cLine + 1 - max 1 (fromIntegral (floor viewL :: Int))
        | otherwise = y
      caretPx = fromIntegral cCell * gCellW g
      tw = textWidth g rect
      -- A drag over the line numbers leaves the caret on the line after the
      -- ones it took, which is no reason to scroll there.
      followX x
        | not caretMoved || isLines drag1 = x
        | caretPx < x = max 0 (caretPx - 4 * gCellW g)
        | caretPx > x + tw - 2 * gCellW g = caretPx - tw + 6 * gCellW g
        | otherwise = x
      scrollY3 = max 0 (min (maxScrollY g rect buf2) (followY scrollY2))
      firstLine = floor scrollY3 :: Int
      lastLine = min (B.lineCount buf2 - 1) (firstLine + ceiling viewL)
      -- Sideways the view goes as far as the widest line on screen.
      widest = maximum (cCell : [B.colToVisual buf2 ln (B.lineLength buf2 ln) | ln <- [firstLine .. lastLine]])
      maxScrollX = max 0 (fromIntegral (widest + 4) * gCellW g - tw)
      scrollX3 = max 0 (min maxScrollX (followX scrollX2))

  -- The lexer state the first line on screen starts in.
  let lexCache = lexCacheFor ed0 buf0 buf2 firstLine
      (_, _, lexStart) = lexCache

  -- The caret shows for half a second after it moved and blinks from then on,
  -- a frame for each blink and none in between.
  let epoch = if caretMoved || drag1 /= DragNone then now else edBlinkEpoch ed0
      phase = floor ((now - epoch) / blinkPeriod) :: Int
      caretOn = focused && even phase
  when focused $ wakeAfter (epoch + fromIntegral (phase + 1) * blinkPeriod - now + 0.005)

  let ed1 =
        ed0
          { edBuffer = buf2
          , edScrollY = scrollY3
          , edScrollX = scrollX3
          , edDrag = drag1
          , edBlinkEpoch = epoch
          , edLexCache = lexCache
          , edReveal = False
          , edViewLines = max 1 (floor viewL - 1)
          , edPressed = (inputMousePressed inp || inputMouseRightPressed inp) && inside
          }
      scene =
        Scene
          { scBuffer = buf2
          , scLang = edLang ed1
          , scLexStart = lexStart
          , scScrollY = scrollY3
          , scScrollX = scrollX3
          , scFontSize = edFontSize ed1
          , scGeometry = g
          , scCaretOn = caretOn
          , scFind = if edFindExact ed1 then edFind ed1 else B.foldCase (edFind ed1)
          , scFindExact = edFindExact ed1
          , scThumbHot = overBar || isThumb drag1
          , scWhitespace = edShowWhitespace ed1
          }
  let part which pointer layout =
        defaultCustomWidgetSpec
          { widgetLayout = layout defaultLayout
          , widgetDraw = \_ r -> drawScene which scene r
          , widgetContent = sceneKey which scene
          , widgetCursor = Just (const pointer)
          , widgetDamageSlop = 0
          }
  resp <- rowWith (grow . gap 0 . padAll 0) $ do
    (respGutter, ()) <- customWidgetWithId widGutter (part PartGutter UiCursorDefault (fillH . fixedW (gGutterW g)))
    (respText, ()) <- customWidgetWithId wid (part PartText UiCursorText grow) {widgetFocusable = True}
    _ <- customWidgetWithId widBar (part PartBar UiCursorDefault (fillH . fixedW scrollBarW))
    pure (respGutter <> respText)
  pure (resp, ed1)
  where
    blinkPeriod = 0.53 :: Double
    isThumb = \case DragThumb _ -> True; _ -> False
    isLines = \case DragLines _ -> True; _ -> False
    isWords = \case DragWords _ _ -> True; _ -> False

-- | The cached lexer state, brought to the first line on screen. A cache
-- from before an edit stands when the edit was below the line it is for; a
-- view that jumped far restarts a little above itself, in the normal state,
-- which is a guess that a block comment longer than that defeats.
lexCacheFor :: Editor -> Buffer -> Buffer -> Int -> (Int, Int, LexState)
lexCacheFor ed before after firstLine
  | stateless = (version, firstLine, LexNormal)
  | usable && cLine <= firstLine = (version, firstLine, advance cLine cState firstLine)
  | otherwise =
      let from = max 0 (firstLine - lookBack)
       in (version, firstLine, advance from LexNormal firstLine)
  where
    lang = edLang ed
    stateless = isNothing (langBlockComment lang) && null (langMultiStrings lang)
    version = B.bufVersion after
    (cVersion, cLine, cState) = edLexCache ed
    editLine = minimum [B.lineOf b o | b <- [before, after], o <- [B.bufCursor b, B.bufAnchor b]]
    usable =
      cVersion >= 0
        && firstLine - cLine <= 4 * lookBack
        && (cVersion == version || (cVersion == B.bufVersion before && editLine > cLine))
    lookBack = 500
    advance !ln !st !to
      | ln >= to = st
      | B.isLongLine after ln = advance (ln + 1) st to
      | otherwise = advance (ln + 1) (lexState lang st (B.lineText after ln)) to

--------------------------------------------------------------------------------
-- Keys
--------------------------------------------------------------------------------

-- | Run the frame's keys and typed characters on the buffer. Chords the
-- application owns (save, open, find and so on) are left alone.
applyKeys :: Ui :> es => Context -> Input -> Int -> Buffer -> Eff es Buffer
applyKeys ctx inp page buf0 = do
  let mods = inputModifiers inp
      shift = modShift mods
      ctrl = modCtrl mods
      alt = modAlt mods
      key b = \case
        KeyLeft -> (if ctrl then B.moveWordLeft else B.moveLeft) shift b
        KeyRight -> (if ctrl then B.moveWordRight else B.moveRight) shift b
        KeyUp
          | alt -> B.moveLines (negate page) shift b
          | otherwise -> B.moveUp shift b
        KeyDown
          | alt -> B.moveLines page shift b
          | otherwise -> B.moveDown shift b
        KeyHome -> (if ctrl then B.moveDocStart else B.moveHome) shift b
        KeyEnd -> (if ctrl then B.moveDocEnd else B.moveEnd) shift b
        KeyBackspace -> (if ctrl then B.deleteWordBack else B.backspace) b
        KeyDelete -> (if ctrl then B.deleteWordForward else B.deleteForward) b
        KeyEnter -> B.newline b
        KeyTab
          | ctrl || alt -> b
          | shift -> B.unindentKey b
          | otherwise -> B.indentKey b
        KeyEscape -> B.setCursor False (B.bufCursor b) b
      buf1 = foldInputKeys key buf0 (inputKeys inp)
      typed = inputChars inp
  if T.null typed
    then pure buf1
    else
      if ctrl && not alt
        then chords (T.unpack typed) buf1
        else
          -- AltGr arrives as Ctrl+Alt, with the key's own letter ahead of the
          -- character it types.
          pure (B.insertText (if ctrl then T.filter (\c -> c > '\x7E' || not (isPlainKey c)) typed else T.filter (>= ' ') typed) buf1)
  where
    isPlainKey c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')
    chords [] b = pure b
    chords (c : cs) b = chord c b >>= chords cs
    chord c b = case c of
      'a' -> pure (B.selectAll b)
      'A' -> pure (B.selectAll b)
      'z' | modShift (inputModifiers inp) -> pure (B.redo b)
      'Z' -> pure (B.redo b)
      'z' -> pure (B.undo b)
      'y' -> pure (B.redo b)
      'c' -> uiIO (clipboardCopy ctx b)
      'x' -> uiIO (clipboardCut ctx b)
      'v' -> uiIO (clipboardPaste ctx b)
      _ -> pure b

-- | Copy, cut and paste through the host's clipboard. With nothing selected,
-- copy and cut take the whole line.
clipboardCopy, clipboardCut, clipboardPaste :: Context -> Buffer -> IO Buffer
-- An empty line has nothing to take, and what the clipboard holds stays.
clipboardCopy ctx b
  | T.null (B.selectedText (orLine b)) = pure b
  | otherwise = b <$ ctxClipboardSet ctx (B.selectedText (orLine b))
clipboardCut ctx b
  | T.null (B.selectedText (orLine b)) = pure b
  | otherwise = do
      copied <- ctxClipboardSet ctx (B.selectedText (orLine b))
      pure (if copied then B.deleteSelection (orLine b) else b)
clipboardPaste ctx b = maybe b (`B.insertText` b) <$> ctxClipboardGet ctx

orLine :: Buffer -> Buffer
orLine b = if B.hasSelection b then b else B.selectLineAt (B.bufCursor b) b

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------

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
      h = foldl' (\acc v -> (acc `xor` v) * 1099511628211) 1469598103934665603 fields
   in if h == 0 then 1 else h
  where
    hashText = T.foldl' (\acc c -> (acc `xor` fromEnum c) * 1099511628211) 1469598103934665603

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
      PartGutter -> FillRect own colGutter : numbers
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
    font = TextFont (scFontSize sc) FontMono WeightNormal FontStyleNormal DecorationNone
    textX = x + gGutterW g + textPad - scScrollX sc
    firstLine = floor (scScrollY sc) :: Int
    yOff = realToFrac (fromIntegral firstLine - scScrollY sc) * lineH
    lastLine = min (B.lineCount buf - 1) (firstLine + ceiling (h / lineH))
    lineY ln = y + yOff + fromIntegral (ln - firstLine) * lineH
    -- The cells on screen, with one to spare on either side.
    firstCell = max 0 (floor (scScrollX sc / cellW) - 1) :: Int
    lastCell = firstCell + ceiling (w / cellW) + 2
    cellX c = textX + fromIntegral c * cellW
    (thumbTop, thumbH) = thumbSpan g rect buf (scScrollY sc)
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
      | otherwise = B.cellOfCol (rowText row) col

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
          let t = if scFindExact sc then rowText row else B.foldCase (rowText row)
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
      | otherwise = marks 0 (T.unpack (T.takeWhile (\c -> c == ' ' || c == '\t') (rowText row)))
      where
        midY = lineY (rowLine row) + fromIntegral (round (lineH / 2) :: Int)
        marks _ [] = []
        marks !cell (c : cs)
          | cell > lastCell = []
          | c == ' ' = [FillRect (Rect (cellX cell + cellW / 2 - 1) (midY - 1) 2 2) colWhitespace | cell >= firstCell] ++ marks (cell + 1) cs
          | otherwise =
              let next = cell + B.tabWidth - cell `rem` B.tabWidth
               in [FillRect (Rect (cellX cell + 2) midY (fromIntegral (next - cell) * cellW - 4) 1) colWhitespace | next > firstCell] ++ marks next cs

    texts = concatMap rowText' rows
    rowText' row
      | rowLong row =
          [DrawTextStyled (cellX firstCell) (lineY (rowLine row)) font (T.map visible (rowText row)) (tokenColor TokPlain) | not (T.null (rowText row))]
      | otherwise = runs (lineY (rowLine row)) 0 (rowText row) (rowSpans row)

    visible c = if c < ' ' then ' ' else c

    -- The draw ops of a line's spans, from a cell on. A run of plain ASCII is
    -- one op; anything else is placed a character at a time, so that the
    -- grid holds whatever a fallback font makes of it.
    runs _ _ _ [] = []
    runs ly cell t (Span n kind : rest)
      | cell > lastCell = []
      | otherwise =
          let seg = T.take n t
              t' = T.drop n t
           in if T.all simple seg
                then
                  let skip = max 0 (firstCell - cell)
                      keep = min n (lastCell - cell + 1) - skip
                      op = DrawTextStyled (cellX (cell + skip)) ly font (T.take keep (T.drop skip seg)) (tokenColor kind)
                   in [op | keep > 0, not (T.all (== ' ') seg)] ++ runs ly (cell + n) t' rest
                else
                  let (ops, cell') = chars ly kind cell seg
                   in ops ++ runs ly cell' t' rest

    simple c = c >= ' ' && c < '\x7F'

    chars ly kind = go []
      where
        go acc !cell t = case T.uncons t of
          Nothing -> (reverse acc, cell)
          Just ('\t', r) -> go acc (cell + B.tabWidth - cell `rem` B.tabWidth) r
          Just (c, r)
            | c <= ' ' -> go acc (cell + 1) r
            | cell < firstCell || cell > lastCell -> go acc (cell + B.charCells c) r
            | otherwise -> go (DrawTextStyled (cellX cell) ly font (T.singleton c) (tokenColor kind) : acc) (cell + B.charCells c) r

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
