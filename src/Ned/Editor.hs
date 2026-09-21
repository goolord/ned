-- | One frame of the editor, as far as the editor can work it out on its own:
-- what the keys did to the text, what the pointer took hold of, where the view
-- ended up, and how far the lexer has to run to reach the top of it.
--
-- It scrolls by itself, in lines, and asks the rope for the lines it shows and
-- no others, so a frame costs the same in a document of ten lines as in one of
-- ten million.
--
-- Nothing here draws. The three widgets the editor is made of, and the ops
-- they build, are in "Ned.View"; what one of their frames runs on is here, the
-- state it runs on is in "Ned.Editor.Types", the measurements in
-- "Ned.Editor.Geometry", and what the keys do in "Ned.Editor.Keys". Read
-- 'editorFrame' top to bottom and it says what one frame of the editor does,
-- in order: keys, pointer, wheel, scroll, lexer, caret.
module Ned.Editor
  ( -- * One frame
    editorFrame
  , EditorFrame (..)

    -- * Its state
  , Editor (..)
  , newEditor
  , revealCaret
  , defaultFontSize

    -- * For the application around it
  , cellWidth
  , clipboardCopy
  , clipboardCut
  , clipboardPaste
  ) where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Effectful (Eff, type (:>))
import NanoUI
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor.Geometry
import Ned.Editor.Keys
import Ned.Editor.Types
import Ned.Highlight
import Ned.Text (clamp)
import Ned.Widget

--------------------------------------------------------------------------------
-- One frame
--------------------------------------------------------------------------------

-- | What a frame of the editor worked out: the editor as the frame leaves it,
-- and the answers the drawing needs that the editor itself does not hold.
data EditorFrame = EditorFrame
  { efEditor :: !Editor
  , efGeometry :: !Geometry
  -- ^ The grid the frame was laid out on.
  , efLexStart :: !LexState
  -- ^ The state the lexer is in at the first line on screen.
  , efCaretOn :: !Bool
  -- ^ Whether the caret shows this frame.
  , efThumbHot :: !Bool
  -- ^ Whether the pointer is over the scrollbar, or holding its thumb.
  }

-- | Run one frame of the editor over the rectangle it is laid out in.
-- @focused@ says the editor has the keyboard, which an application clears
-- while a field of its own is being typed into; @cellW@ and @fm@ are the font
-- it is set in, which whoever lays it out has resolved already.
editorFrame :: Ui :> es => Bool -> Rect -> Float -> FontMetrics -> Editor -> Eff es EditorFrame
editorFrame focused rect cellW fm ed0 = do
  inp <- askInput
  now <- uiTime

  let buf0 = edBuffer ed0
  buf1 <- if focused then applyKeys inp (edViewLines ed0) buf0 else pure buf0

  let g = geometry cellW fm buf1
      mouse = inputMousePos inp
      inside = rectContains rect mouse
      overBar = inside && v2X mouse >= rectX rect + rectW rect - scrollBarW
      overGutter = inside && v2X mouse < rectX rect + gGutterW g
      localY = v2Y mouse - rectY rect
      pointedLine scrollY b =
        clamp 0 (B.lineCount b - 1) (floor (scrollY + realToFrac (localY / gLineH g)))
      -- The offset under the pointer.
      pointed scrollY scrollX b =
        let x = v2X mouse - (rectX rect + gGutterW g + textPad) + scrollX
         in B.offsetAt b (pointedLine scrollY b) (round (x / gCellW g))
      bar = scroller g rect buf1

  -- The pointer: a press starts a selection or takes the thumb, and a held
  -- button carries on with whichever it started.
  let (drag1, buf2, scrollY1)
        | inputMousePressed inp && overBar =
            let grab = thumbGrab bar (edScrollY ed0) localY
             in (DragThumb grab, buf1, thumbScroll bar grab localY)
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
            DragThumb grab -> (DragThumb grab, buf1, thumbScroll bar grab localY)
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
         in edScrollY ed0 + clamp (-3) 3 (over * 0.5)
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
        | otherwise = followRow cLine viewL y
      caretPx = fromIntegral cCell * gCellW g
      tw = textWidth g rect
      -- A drag over the line numbers leaves the caret on the line after the
      -- ones it took, which is no reason to scroll there.
      followX x
        | not caretMoved || isLines drag1 = x
        | caretPx < x = max 0 (caretPx - 4 * gCellW g)
        | caretPx > x + tw - 2 * gCellW g = caretPx - tw + 6 * gCellW g
        | otherwise = x
      scrollY3 = clamp 0 (maxScrollY g rect buf2) (followY scrollY2)
      firstLine = floor scrollY3 :: Int
      lastLine = min (B.lineCount buf2 - 1) (firstLine + ceiling viewL)
      -- Sideways the view goes as far as the widest line on screen.
      widest = maximum (cCell : [B.colToVisual buf2 ln (B.lineLength buf2 ln) | ln <- [firstLine .. lastLine]])
      maxScrollX = max 0 (fromIntegral (widest + 4) * gCellW g - tw)
      scrollX3 = clamp 0 maxScrollX (followX scrollX2)

  -- The lexer state the first line on screen starts in.
  let lexCache = lexCacheFor ed0 buf0 buf2 firstLine
      (_, _, lexStart) = lexCache

  -- The caret shows for half a second after it moved and blinks from then on,
  -- a frame for each blink and none in between.
  let epoch = if caretMoved || drag1 /= DragNone then now else edBlinkEpoch ed0
      phase = floor ((now - epoch) / blinkPeriod) :: Int
      caretOn = focused && even phase
  when focused $ wakeAfter (epoch + fromIntegral (phase + 1) * blinkPeriod - now + 0.005)

  pure
    EditorFrame
      { efEditor =
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
      , efGeometry = g
      , efLexStart = lexStart
      , efCaretOn = caretOn
      , efThumbHot = overBar || isThumb drag1
      }
  where
    blinkPeriod = 0.53 :: Double
    isThumb = \case DragThumb _ -> True; _ -> False
    isLines = \case DragLines _ -> True; _ -> False
    isWords = \case DragWords _ _ -> True; _ -> False

--------------------------------------------------------------------------------
-- The lexer's cache
--------------------------------------------------------------------------------

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
    stateless = isNothing (langBlockComment lang) && null (langMultiStrings lang) && not (langStringGaps lang)
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
