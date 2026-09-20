-- | One frame of the file tree beside the editor: what the pointer landed on,
-- which folders that opens or closes, what the keys walked to, and where the
-- rows ended up scrolled to.
--
-- Like the editor it scrolls by itself, and reads a folder the first time it
-- is opened, so nothing walks a tree nobody looked at.
--
-- Nothing here draws. The panel the rows sit in, and the ops they build, are
-- in "Ned.View"; this turns a frame's pointer and keys into the changes
-- "Ned.FileTree.Model" knows how to make, and hands back the tree, a file that
-- was asked for, and what the drawing needs to know besides.
module Ned.FileTree
  ( -- * One frame
    treeFrame
  , TreeFrame (..)

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
  , minTreeWidth
  ) where

import Control.Monad (when)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Effectful (Eff, type (:>))
-- 'Row' here is a row of the tree, not nano-ui's layout direction.
import NanoUI hiding (Row)
import NanoUI.Monad (askInput)
import Ned.FileTree.Geometry
import Ned.FileTree.Model
import Ned.Text (clamp)
import Ned.Widget

--------------------------------------------------------------------------------
-- One frame
--------------------------------------------------------------------------------

-- | What a frame of the tree worked out: the tree as the frame leaves it, a
-- file its rows were asked to open, and the answers the drawing needs that the
-- tree itself does not hold.
data TreeFrame = TreeFrame
  { tfTree :: !FileTree
  , tfOpened :: !(Maybe FilePath)
  -- ^ A file a click or an Enter asked for.
  , tfHovered :: !Int
  -- ^ The row the pointer is over, or -1.
  , tfThumbHot :: !Bool
  -- ^ Whether the pointer is over the scrollbar, or holding its thumb.
  , tfViewRows :: !Double
  -- ^ How many rows the view holds.
  }

-- | Run one frame of the tree over the rectangle its rows are laid out in.
-- @wantFocus@ says the tree has the keyboard, which the application gives it
-- while the tree is the thing last clicked on; @lineH@ is the height of a row,
-- which whoever lays it out has worked out from the font already.
treeFrame :: Ui :> es => Bool -> Rect -> Float -> FileTree -> Eff es TreeFrame
treeFrame wantFocus rect lineH ft0 = do
  inp <- askInput
  let viewRows = realToFrac (rectH rect / lineH) :: Double
      mouse = inputMousePos inp
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

      hovered = if inside && not overBar then floor (scroll1 + realToFrac (localY / lineH)) else -1

  pure
    TreeFrame
      { tfTree = ft1
      , tfOpened = maybe openedByKey Just openedByMouse
      , tfHovered = if hovered >= 0 && hovered < count1 then hovered else -1
      , tfThumbHot = overBar || isThumb (ftDrag ft1)
      , tfViewRows = viewRows
      }
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
