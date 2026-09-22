-- | One frame of the editor, as far as the editor can work it out on its own:
-- what the keys did to the text, what the pointer took hold of, where the view
-- ended up, and how far the lexer has to run to reach the top of it.
--
-- It scrolls by itself, in lines, and asks the rope for the lines it shows and
-- no others, so a frame costs the same in a document of ten lines as in one of
-- ten million.
--
-- Nothing here draws. The four widgets the editor is made of, and the ops
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
  , efThumbXHot :: !Bool
  -- ^ Whether the pointer is over the sideways bar, or holding its thumb.
  , efWidest :: !Int
  -- ^ The widest the view scrolls sideways to, in cells: the widest line in
  -- the buffer as far as the width scan knows it, with a few of slack past.
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
      -- The editor less its sideways bar, which is the rectangle the text has
      -- to itself and the one the view is worked out over. The whole of the
      -- editor is the pointer's.
      rowRect = Rect (rectX rect) (rectY rect) (rectW rect) (max 1 (rectH rect - scrollBarH))
      mouse = inputMousePos inp
      inside = rectContains rect mouse
      overBar = inside && v2X mouse >= rectX rect + rectW rect - scrollBarW
      overGutter = inside && v2X mouse < rectX rect + gGutterW g
      overHBar = inside && not overBar && v2Y mouse >= rectY rect + rectH rect - scrollBarH
      localX = v2X mouse - rectX rect
      localY = v2Y mouse - rectY rect
      pointedLine scrollY b =
        clamp 0 (B.lineCount b - 1) (floor (scrollY + realToFrac (localY / gLineH g)))
      -- The offset under the pointer.
      pointed scrollY scrollX b =
        let x = v2X mouse - (rectX rect + gGutterW g + textPad) + scrollX
         in B.offsetAt b (pointedLine scrollY b) (round (x / gCellW g))
      bar = scroller g rowRect buf1

  -- The pointer, the wheel and the scroll are one working, for the sideways
  -- thumb and the upright scroll read each other: the lane the thumb travels
  -- is the widest line on screen, and the widest line is where the upright
  -- scroll has put the view.
  --
  -- The pointer: a press starts a selection or takes a thumb, and a held
  -- button carries on with whichever it started.
  let (drag1, buf2, scrollY1)
        | inputMousePressed inp && overBar =
            let grab = thumbGrab bar (edScrollY ed0) localY
             in (DragThumb grab, buf1, thumbScroll bar grab localY)
        | inputMousePressed inp && overHBar =
            -- A press on the lane brings the thumb to the pointer, as the
            -- upright bar's does.
            let grab = thumbGrab hbar (realToFrac (edScrollX ed0)) localX
             in (DragThumbX grab, buf1, edScrollY ed0)
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
            DragThumbX grab -> (DragThumbX grab, buf1, edScrollY ed0)
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

      -- The wheel, three lines a notch; with Shift it scrolls sideways. A
      -- sideways thumb held has the say over both, and goes where the
      -- pointer takes it.
      V2 wheelX wheelY = if inside then inputScroll inp else V2 0 0
      shift = modShift (inputModifiers inp)
      scrollY2 = scrollY1 + realToFrac (if shift then 0 else wheelY) * 3
      scrollX2 = case drag1 of
        DragThumbX grab -> realToFrac (thumbScroll hbar grab localX)
        _ -> edScrollX ed0 + (wheelX + (if shift then wheelY else 0)) * 3 * gCellW g

      -- Follow the caret when it moved, and then keep the scroll within bounds.
      caretMoved =
        B.bufCursor buf2 /= B.bufCursor buf0
          || B.bufVersion buf2 /= B.bufVersion buf0
          || edReveal ed0
      (cLine, cCol) = B.cursorPosition buf2
      cCell = B.colToVisual buf2 cLine cCol
      viewL = viewLinesOf g rowRect
      followY y
        | not caretMoved || autoScrolling || isLines drag1 = y
        | otherwise = followRow cLine viewL y
      caretPx = fromIntegral cCell * gCellW g
      tw = textWidth g rowRect
      -- A drag over the line numbers leaves the caret on the line after the
      -- ones it took, which is no reason to scroll there.
      followX x
        | not caretMoved || isLines drag1 = x
        | caretPx < x = max 0 (caretPx - 4 * gCellW g)
        | caretPx > x + tw - 2 * gCellW g = caretPx - tw + 6 * gCellW g
        | otherwise = x
      scrollY3 = clamp 0 (maxScrollY g rowRect buf2) (followY scrollY2)
      firstLine = floor scrollY3 :: Int
      lastLine = min (B.lineCount buf2 - 1) (firstLine + ceiling viewL)
      -- Sideways the view goes as far as the widest line in the buffer, and
      -- a few cells past, so the bar stays under it however far it is
      -- scrolled -- the widest one off screen is still to be walked to. The
      -- lines on screen are measured at once; the rest of the buffer a scan
      -- gets through a piece a frame, which an edit starts over, and what
      -- the scan has measured so far is what the view is held to. A line
      -- deleted can leave it a few frames too wide for the text, and the
      -- scan's next round puts that right.
      seen = maximum (cCell : [B.colToVisual buf2 ln (B.lineLength buf2 ln) | ln <- [firstLine .. lastLine]])
      restart = B.bufVersion buf2 /= edWidestVer ed0
      scanFrom
        | restart = 0
        | otherwise = min (edWidestScan ed0) (B.lineCount buf2)
      walk !ln !left !acc
        | ln >= B.lineCount buf2 || left <= 0 = (ln, acc)
        | otherwise =
            let len = B.lineLength buf2 ln
             in walk (ln + 1) (left - len) $! max acc (B.colToVisual buf2 ln len)
      (scanTo, scanFound) = walk scanFrom scanBudget (if restart then seen else edWidest ed0)
      widest = max seen scanFound
      hbar = hscroller g rowRect (widest + 4)
      maxScrollX = maxScrollXOf g rowRect (widest + 4)
      scrollX3 = clamp 0 maxScrollX (followX scrollX2)

  when autoScrolling (wakeAfter 0.03)

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
            , edWidestVer = B.bufVersion buf2
            , edWidest = scanFound
            , edWidestScan = scanTo
            , edPressed = (inputMousePressed inp || inputMouseRightPressed inp) && inside
            }
      , efGeometry = g
      , efLexStart = lexStart
      , efCaretOn = caretOn
      , efThumbHot = overBar || isThumb drag1
      , efThumbXHot = overHBar || isThumbX drag1
      , efWidest = widest
      }
  where
    blinkPeriod = 0.53 :: Double
    -- The piece of the buffer, in characters, a frame's width scan walks:
    -- enough to have a big file measured in a few frames, little enough to
    -- stay off the frame's back.
    scanBudget = 262144 :: Int
    isThumb = \case DragThumb _ -> True; _ -> False
    isThumbX = \case DragThumbX _ -> True; _ -> False
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
