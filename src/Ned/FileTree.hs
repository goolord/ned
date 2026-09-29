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
-- was asked for, what the keys that are the application's own asked of it, and
-- what the drawing needs to know besides.
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

    -- * Where it puts things
  , treeIndent
  , treeChevron
  , treeBarW
  , treeHeaderPad
  , treeScroller
  ) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import qualified Data.Text as T
-- 'Row' here is a row of the tree, not nano-ui's layout direction.
import NanoUI hiding (Row)
import Ned.Editor.Vim (P (..), Request, appStep)
import Ned.FileTree.Model
import Ned.Text (clamp)
import Ned.Widget

--------------------------------------------------------------------------------
-- One frame
--------------------------------------------------------------------------------

-- | What a frame of the tree worked out: the tree as the frame leaves it, a
-- file its rows were asked to open, what vim's application keys asked for,
-- and the answers the drawing needs that the tree itself does not hold.
data TreeFrame = TreeFrame
  { tfTree :: !FileTree
  , tfOpened :: !(Maybe FilePath)
  -- ^ A file a click or an Enter asked for.
  , tfRequest :: ![Request]
  -- ^ What the keys that ask the application asked for, which the tree leaves
  -- to it as the editor's own vim does.
  , tfHovered :: !Int
  -- ^ The row the pointer is over, or -1.
  , tfThumbHot :: !Bool
  -- ^ Whether the pointer is over the scrollbar, or holding its thumb.
  , tfViewRows :: !Double
  -- ^ How many rows the view holds.
  }

-- | Run one frame of the tree over the rectangle its rows are laid out in.
-- @vim@ says vim's keys are on, which walk the tree as well as the arrows do;
-- @wantFocus@ says the tree has the keyboard, which the application gives it
-- while the tree is the thing last clicked on; @lineH@ is the height of a row,
-- which whoever lays it out has worked out from the font already.
treeFrame :: Bool -> Bool -> Rect -> Float -> FileTree -> NanoUI TreeFrame
treeFrame vim wantFocus rect lineH ft0 = do
  inp <- askInput
  let viewRows = realToFrac (rectH rect / lineH) :: Double
      mouse = inputMousePos inp
      inside = rectContains rect mouse
      localY = v2Y mouse - rectY rect
      rowsNow = ftRows ft0
      rowCount = sizeofSmallArray rowsNow
      overBar = inside && v2X mouse >= rectX rect + rectW rect - treeBarW && fromIntegral rowCount > viewRows
      bar = treeScroller rect rowCount viewRows
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

  -- A directory opened by this frame's click is read before the frame draws it.
  ftLoaded <- liftIO (loadPending ftMouse)

  -- The keyboard, when the tree has it. With vim's keys, what was typed
  -- goes after the named keys.
  let named = foldInputKeys applyKey (ftLoaded, Nothing, []) (inputKeys inp)
      halfView = max 1 (floor (viewRows / 2))
      (ftKeys, openedByKey, asked)
        | not wantFocus = (ftLoaded, Nothing, [])
        | vim = foldl' (vimKey halfView) named (vimTyped inp)
        | otherwise = named

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
      , tfRequest = asked
      , tfHovered = if hovered >= 0 && hovered < count1 then hovered else -1
      , tfThumbHot = overBar || isThumb (ftDrag ft1)
      , tfViewRows = viewRows
      }
  where
    isThumb = \case DragThumb _ -> True; _ -> False

    -- Up and down walk the rows, left closes a folder or steps out to the one
    -- above, right opens one or steps into it, and Enter opens a file. A named
    -- key is no part of an application key, so it gives one up, and Escape
    -- gives up the count and the g that were waiting as well.
    applyKey (ft0', op, ask) k =
      let ft = ft0' {ftAppKeys = Nothing}
          rows = ftRows ft
          n = sizeofSmallArray rows
          here = selectedRow ft
          sel = rowAt rows here
       in case k of
            KeyUp -> (selectRow (if here < 0 then n - 1 else here - 1) ft, op, ask)
            KeyDown -> (selectRow (if here < 0 then 0 else here + 1) ft, op, ask)
            KeyHome -> (selectRow 0 ft, op, ask)
            KeyEnd -> (selectRow (n - 1) ft, op, ask)
            KeyLeft -> case sel of
              Just r | rowOpen r -> (toggle (rowPath r) ft, op, ask)
              Just r -> (selectRow (parentOf rows here (rowDepth r)) ft, op, ask)
              Nothing -> (ft, op, ask)
            KeyRight -> case sel of
              Just r | rowDir r && not (rowOpen r) -> (toggle (rowPath r) ft, op, ask)
              Just r | rowDir r -> (selectRow (here + 1) ft, op, ask)
              _ -> (ft, op, ask)
            KeyEnter -> case sel of
              Just r | rowDir r -> (toggle (rowPath r) ft, op, ask)
              Just r -> (ft, Just (rowPath r), ask)
              Nothing -> (ft, op, ask)
            KeyEscape -> (ft {ftVimPending = ""}, op, ask)
            _ -> (ft, op, ask)

    -- What vim's keys typed: the characters, or with Ctrl held the half-view
    -- steps, which type nothing.
    vimTyped inp
      | modCtrl mods && not (modAlt mods) = [c' | KeyChar c <- keys, Just c' <- [control c]]
      | otherwise = T.unpack (inputChars inp)
      where
        mods = inputModifiers inp
        keys = reverse (foldInputKeys (flip (:)) [] (inputKeys inp))
        control = \case
          'd' -> Just '\EOT'
          'u' -> Just '\NAK'
          _ -> Nothing

    -- Vim's keys, one at a time. The keys that ask the application and touch
    -- no text -- the leader's and the rest -- mean as much with the keyboard
    -- in a tree as in the text, so they are read out of vim's own table and
    -- left to it: a key that finishes one asks for something, a key another
    -- may still begin waits for the next, and a key that is neither gives the
    -- keys so far up and is the tree's own after all.
    vimKey half (ft0', op, asks) c =
      let ks = fromMaybe "" (ftAppKeys ft0')
          ft = ft0' {ftAppKeys = Nothing}
       in case appStep (ks <> [c]) of
            Got rs -> (ft, op, asks ++ rs)
            More -> (ft0' {ftAppKeys = Just (ks <> [c])}, op, asks)
            Bad -> foldl' (treeKey half) (ft, op, asks) (ks <> [c])

    -- j and k walk the rows, and Ctrl+D and Ctrl+U half a view; h and l are
    -- Left and Right, save that l on a file opens it; o opens a file or a
    -- folder as Enter does, and O the same, whose Shift opens a file in a tab
    -- of its own as a Shift+click does; gg and G go to the first row and the
    -- last, or with a count to that row; and - puts the root on the folder
    -- above, on the folder it was. A count before j, k and the half views
    -- goes that many times as far.
    treeKey half (ft0', op, ask) c
      | isDigit c && (c /= '0' || not (null digits)) && null prefix =
          (ft0' {ftVimPending = take 6 (pending ++ [c])}, op, ask)
      | otherwise = case (prefix, c) of
          ("g", 'g') -> (goRow (maybe 0 (subtract 1) counted), op, ask)
          ("", 'g') -> (ft {ftVimPending = pending ++ "g"}, op, ask)
          ("", 'G') -> (goRow (maybe (n - 1) (subtract 1) counted), op, ask)
          ("", 'j') -> (walk count, op, ask)
          ("", 'k') -> (walk (negate count), op, ask)
          ("", '\EOT') -> (walk (count * half), op, ask)
          ("", '\NAK') -> (walk (negate (count * half)), op, ask)
          ("", 'h') -> applyKey (ft, op, ask) KeyLeft
          ("", 'l') -> case rowAt rows here of
            Just r | not (rowDir r) -> (ft, Just (rowPath r), ask)
            _ -> applyKey (ft, op, ask) KeyRight
          ("", 'o') -> applyKey (ft, op, ask) KeyEnter
          ("", 'O') -> applyKey (ft, op, ask) KeyEnter
          ("", '-') | hasParentRoot ft -> ((parentRoot ft) {ftSelected = Just (ftRoot ft), ftReveal = True}, op, ask)
          _ -> (ft, op, ask)
      where
        pending = ftVimPending ft0'
        ft = ft0' {ftVimPending = ""}
        (digits, prefix) = span isDigit pending
        counted = if null digits then Nothing else Just (read digits :: Int)
        count = fromMaybe 1 counted
        rows = ftRows ft
        n = sizeofSmallArray rows
        here = selectedRow ft
        goRow i = selectRow (clamp 0 (n - 1) i) ft
        -- From no row at all, down starts at the first and up at the last.
        walk by
          | here < 0 = goRow (if by > 0 then by - 1 else n + by)
          | otherwise = goRow (here + by)

    -- The row the one at @i@ sits under: the first one above it that is a
    -- step shallower.
    parentOf rows i depth = go (i - 1)
      where
        go j
          | j < 0 = -1
          | rowDepth (indexSmallArray rows j) < depth = j
          | otherwise = go (j - 1)

--------------------------------------------------------------------------------
-- Where it puts things
--------------------------------------------------------------------------------

-- The panel here hit-tests against these and the drawing in "Ned.View.Tree"
-- places everything by them. What a row shares with the finder's rows -- its
-- height, its margin, its icon's column and its mark -- is "Ned.Widget"'s, and
-- what colour any of it is "Ned.Theme"'s.

-- | How far a row indents for each folder it is in, the column a folder's
-- chevron takes, and the lane of the scrollbar. A file has no chevron but
-- still leaves room for one, so that the icons of a folder and of the files
-- inside it line up in a column.
treeIndent, treeChevron, treeBarW :: Float
treeIndent = 14
treeChevron = 14
treeBarW = 10

-- | Where the panel's header sets the root's name: the column a row's icon
-- starts in, so that the name of the folder lines up with what is under it
-- rather than sitting against the panel's edge.
treeHeaderPad :: Float
treeHeaderPad = rowPad + treeChevron + 2

maxScroll :: Int -> Double -> Double
maxScroll rowCount viewRows = max 0 (fromIntegral rowCount - viewRows)

-- | The view and its scrollbar, over so many rows.
treeScroller :: Rect -> Int -> Double -> Scroller
treeScroller rect rowCount viewRows =
  Scroller (rectH rect) (viewRows / max 1 (fromIntegral rowCount)) (maxScroll rowCount viewRows)
