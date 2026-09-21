-- | The file tree's panel, and the draw ops of the rows on screen.
--
-- The panel is the root's name over one custom nano-ui widget that holds every
-- row and scrolls itself, which is why a tree of ten thousand files draws the
-- twenty that are on screen and no others. The bar that resizes the panel is
-- the pane grid's, in "Ned.View"; this is only what the pane holds.
--
-- Nothing here reads input or keeps state. What a frame of the tree works out
-- is "Ned.FileTree"'s, and what it hands back, together with the tree itself,
-- is gathered into a 'TreeScene'; 'treeSceneKey' is a number over the same
-- values, so a frame whose rows have not changed builds no ops. How far apart
-- any of it sits is "Ned.FileTree.Geometry"'s to say, and what colour it is
-- "Ned.Theme"'s.
module Ned.View.Tree
  ( fileTreePanel
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
-- 'Row' here is a row of the tree, not nano-ui's layout direction.
import NanoUI hiding (Row)
import Ned.FileTree
import qualified Ned.FileTree.Geometry as TG
import Ned.Theme (TreeColors (..), languageTint, treeColors)
import Ned.Widget (fileIcon, folderIcon, thumbSpan)
import System.FilePath (equalFilePath)

--------------------------------------------------------------------------------
-- The file tree
--------------------------------------------------------------------------------

-- | The panel: the root's name, the rows, and nothing else. Pass the tree and
-- keep the result; the response is for hanging a context menu on, and the path
-- is a file the rows were asked to open.
--
-- It takes the keyboard when @wantFocus@ is set, which the application does
-- while the tree is the thing last clicked on. The bar that resizes the panel
-- is the pane grid's, above; this is only what the pane holds.
fileTreePanel :: Ui :> es => Bool -> Maybe FilePath -> FileTree -> Eff es (Response, FileTree, Maybe FilePath)
fileTreePanel wantFocus current ft0 =
  columnWith (tight . gap 0 . fillW . fillH) $ do
    -- The padding goes on last: 'tight' before it would take it off again,
    -- and the name is meant to start where the rows' own names do. It is
    -- set at full strength and semibold: it names the thing the panel is
    -- about, and muted grey had it reading as a row that could not be
    -- clicked.
    rowWith (padXY TG.treeHeaderPad 6 . tight . fillW . gap 4 . alignMid) $
      labelWith (tight . fontSemiBold) (rootName ft0)
    separator
    treeRows wantFocus current ft0

-- | The rows, in one widget that scrolls itself.
treeRows :: Ui :> es => Bool -> Maybe FilePath -> FileTree -> Eff es (Response, FileTree, Maybe FilePath)
treeRows wantFocus current ft0 = do
  wid <- nextId
  lineH <- TG.rowHeight <$> uiFontMetrics
  rect <- fromMaybe (Rect 0 0 defaultTreeWidth 600) <$> lastRect wid

  -- The tree keeps the keyboard for as long as it is the thing being used, as
  -- the editor does with its own.
  when wantFocus (holdFocus wid)

  tf <- treeFrame wantFocus rect lineH ft0
  let ft1 = tfTree tf
      scene =
        TreeScene
          { tsRows = ftRows ft1
          , tsVersion = ftVersion ft1
          , tsScroll = ftScroll ft1
          , tsLineH = lineH
          , tsViewRows = tfViewRows tf
          , tsSelected = ftSelected ft1
          , tsCurrent = current
          , tsHovered = tfHovered tf
          , tsFocused = wantFocus
          , tsThumbHot = tfThumbHot tf
          }

  (resp, ()) <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (grow . fillH) defaultLayout
        , widgetDraw = \cdc r -> drawTree cdc scene r
        , widgetContent = treeSceneKey scene
        , widgetCursor = Just (const UiCursorDefault)
        , widgetFocusable = True
        , widgetDamageSlop = 0
        , -- Every row is this one widget, so a pointer moving from one row
          -- to the next would otherwise ask for no frame, and the row drawn
          -- under it would stay where it was until something else wanted
          -- one. The hovered row is in the key, so a move within a row
          -- repaints nothing.
          widgetTrackPointer = True
        }
  pure (resp, ft1, tfOpened tf)

-- | Everything the tree's drawing reads.
data TreeScene = TreeScene
  { tsRows :: !(SmallArray Row)
  , tsVersion :: !Int
  , tsScroll :: !Double
  , tsLineH :: !Float
  , tsViewRows :: !Double
  , tsSelected :: !(Maybe FilePath)
  , tsCurrent :: !(Maybe FilePath)
  , tsHovered :: !Int
  , tsFocused :: !Bool
  , tsThumbHot :: !Bool
  }

-- | A number that changes when what the tree draws does. The version stands
-- for the rows, which are worked out only when they change.
treeSceneKey :: TreeScene -> Int
treeSceneKey sc =
  contentKeyOf
    [ keyPart (tsVersion sc)
    , keyPart (tsScroll sc)
    , keyPart (tsSelected sc)
    , keyPart (tsCurrent sc)
    , keyPart (tsHovered sc)
    , keyPart (tsFocused sc)
    , keyPart (tsThumbHot sc)
    ]

-- | The draw ops of the rows on screen, and of the scrollbar over them when
-- there is more than the view holds.
drawTree :: CustomDrawContext -> TreeScene -> Rect -> SmallArray DrawOp
drawTree cdc sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect (tcPanel tc) : concatMap rowOps [first .. last'] ++ bar)
  where
    theme = cdcTheme cdc
    tc = treeColors theme
    fm = cdcFont cdc
    lineH = tsLineH sc
    rows = tsRows sc
    count = sizeofSmallArray rows
    first = max 0 (floor (tsScroll sc))
    last' = min (count - 1) (first + ceiling (h / lineH))
    yOff = realToFrac (fromIntegral first - tsScroll sc) * lineH
    rowY i = y + yOff + fromIntegral (i - first) * lineH
    textY ry = ry + (lineH - fmLineHeight fm) / 2
    font weight = TextFont 0 FontRegular weight FontStyleNormal DecorationNone
    -- The lane the scrollbar has, which is nothing until there is more to
    -- show than the view holds.
    lane = if fromIntegral count > tsViewRows sc then TG.treeBarW else 0
    -- A row's own colours are in 'TreeColors'; the tint on a file's page is
    -- the family of language it would open as.
    rowOps i =
      let r = indexSmallArray rows i
          ry = rowY i
          indent = TG.treePad + fromIntegral (rowDepth r) * TG.treeIndent
          selected = maybe False (equalFilePath (rowPath r)) (tsSelected sc)
          -- The file the editor has, which is a path the application made and
          -- not one of ours, so it is matched the way the platform would.
          isCurrent = maybe False (equalFilePath (rowPath r)) (tsCurrent sc)
          -- What a pick lands on is inset rather than run from edge to edge,
          -- so the panel keeps a margin down both sides and the mark has
          -- somewhere of its own to sit.
          pick = Rect (x + TG.treeMark + 2) (ry + 1) (max 0 (w - TG.treeMark - 4 - lane)) (lineH - 2)
          backdrop
            | selected = [FillRoundedRect pick TG.treeRadius (if tsFocused sc then tcPicked tc else tcPickedAway tc)]
            | tsHovered sc == i = [FillRoundedRect pick TG.treeRadius (tcHover tc)]
            | otherwise = []
          -- One rule for each folder this row sits inside, down the middle of
          -- that folder's own chevron. This is what makes the rows a tree
          -- rather than a list of names: at the top level there is one root
          -- and so no rule at all.
          spine =
            [ FillRect (Rect (rule k) ry 1 lineH) (tcSpine tc)
            | k <- [0 .. rowDepth r - 1]
            ]
          rule k = fromIntegral (round (x + TG.treePad + fromIntegral k * TG.treeIndent + TG.treeChevron / 2) :: Int)
          mark = [FillRect (Rect x (ry + 1) TG.treeMark (lineH - 2)) (tcCurrent tc) | isCurrent]
          -- A folder has a chevron pointing along or down; a file has none.
          cy = ry + lineH / 2
          chevron
            | not (rowDir r) = []
            | otherwise =
                let cx = x + indent + TG.treeChevron / 2
                 in [ if rowOpen r
                        then FillTriangle (cx - 5) (cy - 2.5) (cx + 5) (cy - 2.5) cx (cy + 3.5) (tcMuted tc)
                        else FillTriangle (cx - 2.5) (cy - 5) (cx - 2.5) (cy + 5) (cx + 3.5) cy (tcMuted tc)
                    ]
          icon
            | rowDir r = folderIcon (x + indent + TG.treeChevron) cy (if rowOpen r then tcName tc else tcMuted tc)
            | otherwise = fileIcon (x + indent + TG.treeChevron) cy (languageTint theme (rowPath r))
          tx = x + indent + TG.treeChevron + TG.treeIcon
          avail = w - (tx - x) - TG.treePad - lane
          (weight, color)
            | isCurrent = (WeightSemiBold, tcCurrent tc)
            | rowDir r = (WeightSemiBold, if rowOpen r then tcName tc else tcMuted tc)
            | otherwise = (WeightNormal, tcName tc)
       in backdrop ++ spine ++ mark ++ chevron ++ icon ++ [DrawTextStyled tx (textY ry) (font weight) (elide fm avail (rowName r)) color]

    bar
      | lane <= 0 = []
      | otherwise =
          let (thumbTop, thumbH) = thumbSpan (TG.scroller rect count (tsViewRows sc)) (tsScroll sc)
           in [ FillRoundedRect
                  (Rect (x + w - TG.treeBarW + 2) (y + thumbTop + 2) (TG.treeBarW - 4) (thumbH - 4))
                  3
                  (if tsThumbHot sc then tcThumbHot tc else tcThumb tc)
              ]

-- | A name cut to fit, with an ellipsis where it was cut.
elide :: FontMetrics -> Float -> Text -> Text
elide fm avail name
  | avail <= 0 = T.empty
  | full <= avail = name
  | otherwise = go (min (T.length name - 1) (max 1 (floor (avail / max 1 (full / fromIntegral (T.length name))))))
  where
    full = lineWidth fm name
    ellipsis = T.singleton '\x2026'
    go n
      | n <= 0 = ellipsis
      | lineWidth fm (T.take n name <> ellipsis) <= avail = T.take n name <> ellipsis
      | otherwise = go (n - 1)
