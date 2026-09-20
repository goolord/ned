-- | The file tree beside the editor: the folder the open file is in, with its
-- directories opening and closing and a click on a file opening it.
--
-- Like the editor, it is a nano-ui custom widget that scrolls by itself and
-- whose draw ops are keyed on everything it reads, so a frame in which
-- nothing changed builds nothing.
--
-- This module is the panel: it turns a frame's pointer and keys into the
-- changes "Ned.FileTree.Model" knows how to make, and hands what comes of
-- that to "Ned.FileTree.Draw". The application above it sees a tree, a
-- response to hang a menu on, and a file that was asked for.
module Ned.FileTree
  ( -- * The panel
    fileTreePanel

    -- * The tree it is given
  , FileTree (..)
  , Row (..)
  , newFileTree
  , setRoot
  , parentRoot
  , hasParentRoot
  , reveal
  , refresh
  , collapseAll
  , rootName
  , defaultTreeWidth
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Effectful (Eff, type (:>))
-- 'Row' here is a row of the tree, not nano-ui's layout direction.
import NanoUI hiding (Row)
import NanoUI.Context (Context (..), getPrevRect)
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Monad (askContext, askInput)
import Ned.FileTree.Draw
import Ned.FileTree.Model
import Ned.Text (clamp)
import Ned.Widget

--------------------------------------------------------------------------------
-- The panel
--------------------------------------------------------------------------------

-- | The panel: the root's name, the rows, and the bar that resizes it. Pass
-- the tree and keep the result; the response is for hanging a context menu
-- on, and the path is a file the rows were asked to open.
--
-- It takes the keyboard when @wantFocus@ is set, which the application does
-- while the tree is the thing last clicked on.
fileTreePanel :: Ui :> es => Bool -> Maybe FilePath -> FileTree -> Eff es (Response, FileTree, Maybe FilePath)
fileTreePanel wantFocus current ft0 =
  rowWith (tight . gap 0 . fillH) $ do
    (resp, ft1, opened) <- columnWith (tight . gap 0 . fillH . fixedW (ftWidth ft0)) $ do
      -- The padding goes on last: 'tight' before it would take it off again,
      -- and the name is meant to start where the rows' own names do. It is
      -- set at full strength and semibold: it names the thing the panel is
      -- about, and muted grey had it reading as a row that could not be
      -- clicked.
      rowWith (padXY treeHeaderPad 6 . tight . fillW . gap 4 . alignMid) $
        labelWith (tight . fontSemiBold) (rootName ft0)
      separator
      treeRows wantFocus current ft0
    ft2 <- splitterBar ft1
    pure (resp, ft2, opened)

-- | The rows, in one widget that scrolls itself.
treeRows :: Ui :> es => Bool -> Maybe FilePath -> FileTree -> Eff es (Response, FileTree, Maybe FilePath)
treeRows wantFocus current ft0 = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  let fm = ctxFontMetrics ctx
      lineH = rowHeight fm
  prev <- uiIO (getPrevRect ctx wid)
  let rect = fromMaybe (Rect 0 0 (ftWidth ft0) 600) prev
      viewRows = realToFrac (rectH rect / lineH) :: Double

  -- The tree keeps the keyboard for as long as it is the thing being used, as
  -- the editor does with its own.
  when wantFocus $ uiIO (takeFocus ctx wid)

  let mouse = inputMousePos inp
      inside = rectContains rect mouse
      localY = v2Y mouse - rectY rect
      rowsNow = ftRows ft0
      rowCount = sizeofSmallArray rowsNow
      overBar = inside && v2X mouse >= rectX rect + rectW rect - treeBarW && fromIntegral rowCount > viewRows
      bar = scroller rect rowCount viewRows
      pointedRow = floor (ftScroll ft0 + realToFrac (localY / lineH)) :: Int

      pressedNow = (inputMousePressed inp || inputMouseRightPressed inp) && inside

      -- The pointer. A press on a folder opens or closes it, and one on a
      -- file opens the file.
      (ftMouse, openedByMouse)
        | inputMousePressed inp && overBar =
            let grab = thumbGrab bar (ftScroll ft0) localY
             in (ft0 {ftDrag = DragThumb grab, ftScroll = thumbScroll bar grab localY}, Nothing)
        | pressedNow =
            case rowAt rowsNow pointedRow of
              Nothing -> (ft0, Nothing)
              Just hit ->
                let picked = ft0 {ftSelected = Just (rowPath hit)}
                 in if inputMouseRightPressed inp
                      then (picked, Nothing)
                      else
                        if rowDir hit
                          then (toggle (rowPath hit) picked, Nothing)
                          else (picked, Just (rowPath hit))
        | not (inputMouseDown inp) =
            (case ftDrag ft0 of DragThumb _ -> ft0 {ftDrag = DragNone}; _ -> ft0, Nothing)
        | otherwise = case ftDrag ft0 of
            DragThumb grab -> (ft0 {ftScroll = thumbScroll bar grab localY}, Nothing)
            _ -> (ft0, Nothing)

  -- nano-ui runs a frame for a pointer that only moved when it came over
  -- another widget. Every row here is the same widget, so a pointer crossing
  -- from one row to the next asks for nothing, and the row drawn under it
  -- would stay where it was until something else wanted a frame: in an editor
  -- that sleeps between caret blinks, half a second. While the pointer is
  -- over the tree it asks for its own frames. A frame whose rows have not
  -- changed builds no draw ops and repaints nothing, so this costs the wake
  -- and no more, and it stops as soon as the pointer leaves.
  when inside (wakeAfter 0.03)

  -- A directory opened by this frame's click is read before the frame draws it.
  ftLoaded <- uiIO (loadPending ftMouse)

  -- The keyboard, when the tree has it.
  let (ftKeys, openedByKey)
        | not wantFocus = (ftLoaded, Nothing)
        | otherwise = foldInputKeys applyKey (ftLoaded, Nothing) (inputKeys inp)

      -- The wheel, three rows a notch.
      V2 _ wheelY = if inside then inputScroll inp else V2 0 0
      scrolled = ftScroll ftKeys + realToFrac wheelY * 3

      -- Follow the selection when it was just asked for, and then keep the
      -- scroll within what there is to show.
      rows1 = ftRows ftKeys
      count1 = sizeofSmallArray rows1
      sel1 = selectedRow ftKeys
      follow y = if ftReveal ftKeys && sel1 >= 0 then followRow sel1 viewRows y else y
      scroll1 = clamp 0 (maxScroll count1 viewRows) (follow scrolled)

      ft1 =
        ftKeys
          { ftScroll = scroll1
          , -- The row asked for is either on screen now or was never there to
            -- find; either way the ask is done with, unless a directory above
            -- it is still to be read.
            ftReveal = ftReveal ftKeys && sel1 < 0 && not (null (ftPending ftKeys))
          , ftPressed = pressedNow
          }
      opened = maybe openedByKey Just openedByMouse

      hovered = if inside && not overBar then floor (scroll1 + realToFrac (localY / lineH)) else -1
      scene =
        Scene
          { scRows = rows1
          , scVersion = ftVersion ft1
          , scScroll = scroll1
          , scLineH = lineH
          , scViewRows = viewRows
          , scSelected = ftSelected ft1
          , scCurrent = current
          , scHovered = if hovered >= 0 && hovered < count1 then hovered else -1
          , scFocused = wantFocus
          , scThumbHot = overBar || isThumb (ftDrag ft1)
          }

  (resp, ()) <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (grow . fillH) defaultLayout
        , widgetDraw = \cdc r -> drawTree cdc scene r
        , widgetContent = sceneKey scene
        , widgetCursor = Just (const UiCursorDefault)
        , widgetFocusable = True
        , widgetDamageSlop = 0
        }
  pure (resp, ft1, opened)
  where
    isThumb = \case DragThumb _ -> True; _ -> False

    -- Up and down walk the rows, left closes a folder or steps out to the one
    -- above, right opens one or steps into it, and Enter opens a file.
    applyKey (ft, op) k =
      let rows = ftRows ft
          n = sizeofSmallArray rows
          here = selectedRow ft
          sel = rowAt rows here
       in case k of
            KeyUp -> (selectRow (if here < 0 then n - 1 else here - 1) ft, op)
            KeyDown -> (selectRow (if here < 0 then 0 else here + 1) ft, op)
            KeyHome -> (selectRow 0 ft, op)
            KeyEnd -> (selectRow (n - 1) ft, op)
            KeyLeft -> case sel of
              Just r | rowOpen r -> (toggle (rowPath r) ft, op)
              Just r -> (selectRow (parentOf rows here (rowDepth r)) ft, op)
              Nothing -> (ft, op)
            KeyRight -> case sel of
              Just r | rowDir r && not (rowOpen r) -> (toggle (rowPath r) ft, op)
              Just r | rowDir r -> (selectRow (here + 1) ft, op)
              _ -> (ft, op)
            KeyEnter -> case sel of
              Just r | rowDir r -> (toggle (rowPath r) ft, op)
              Just r -> (ft, Just (rowPath r))
              Nothing -> (ft, op)
            _ -> (ft, op)

    -- The row the one at @i@ sits under: the first one above it that is a
    -- step shallower.
    parentOf rows i depth = go (i - 1)
      where
        go j
          | j < 0 = -1
          | rowDepth (indexSmallArray rows j) < depth = j
          | otherwise = go (j - 1)

--------------------------------------------------------------------------------
-- The splitter
--------------------------------------------------------------------------------

-- | The bar between the tree and the editor, which drags to resize it.
splitterBar :: Ui :> es => FileTree -> Eff es FileTree
splitterBar ft0 = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  prev <- uiIO (getPrevRect ctx wid)
  let rect = fromMaybe (Rect 0 0 splitterW 600) prev
      mouse = inputMousePos inp
      over = rectContains rect mouse
      ft1
        | inputMousePressed inp && over =
            -- Where the pointer is from the tree's edge when it takes the bar:
            -- a drag puts the edge there again, wherever the pointer goes.
            ft0 {ftDrag = DragWidth (v2X mouse - ftWidth ft0)}
        | not (inputMouseDown inp) = case ftDrag ft0 of
            DragWidth _ -> ft0 {ftDrag = DragNone}
            _ -> ft0
        | otherwise = case ftDrag ft0 of
            -- The width follows the pointer from where it took the bar. The
            -- bar's own rect is a frame behind the width and moves with it, so
            -- measuring against it would chase itself; the press's offset from
            -- the edge does not, and the bar settles under the pointer as the
            -- layout catches up.
            DragWidth grab ->
              ft0 {ftWidth = clamp minTreeWidth maxTreeWidth (v2X mouse - grab)}
            _ -> ft0
      hot = over || isWidth (ftDrag ft1)
  _ <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (fillH . fixedW splitterW) defaultLayout
        , widgetDraw = \cdc r -> splitterOps cdc hot r
        , widgetContent = contentKey [if hot then 1 else 0]
        , widgetCursor = Just (const UiCursorEwResize)
        , widgetDamageSlop = 0
        }
  pure ft1
  where
    isWidth = \case DragWidth _ -> True; _ -> False
