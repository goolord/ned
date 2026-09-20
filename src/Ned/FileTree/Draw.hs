-- | How the file tree looks: how far apart it sets things, and the draw ops
-- of the rows on screen.
--
-- The panel next door hit-tests against a few of the measurements here (the
-- height of a row and the lane the scrollbar has), so they are written down
-- once and both read them. What colour any of it is is "Ned.Theme"'s to say.
module Ned.FileTree.Draw
  ( -- * Measurements
    treeHeaderPad
  , treeHeaderHeight
  , treeBarW
  , rowHeight
  , maxScroll
  , scroller

    -- * Drawing
  , Scene (..)
  , sceneKey
  , drawTree
  ) where

import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
-- 'Row' here is a row of the tree, not nano-ui's layout direction.
import NanoUI hiding (Row)
import Ned.FileTree.Model
import Ned.Theme (TreeColors (..), languageTint, treeColors)
import Ned.Widget (Scroller (..), contentHash, hashText, thumbSpan)
import System.FilePath (equalFilePath)

--------------------------------------------------------------------------------
-- Measurements
--------------------------------------------------------------------------------

-- How far apart the panel sets things. What colour any of it is is
-- "Ned.Theme"'s to say, in 'treeColors': the tree is chrome, so its greys are
-- worked out from whatever theme is running rather than written down here.

treePad, treeIndent, treeChevron, treeIcon, treeBarW, treeMark :: Float
treePad = 8
treeIndent = 14
treeChevron = 14

-- | The column an icon sits in, between the chevron and the name. A file has
-- no chevron but still leaves room for one, so that the icons of a folder and
-- of the files inside it line up in a column.
treeIcon = 18
treeBarW = 10

-- | The bar down the left of the row whose file the editor has open. It is its
-- own mark and not the selection's, because arrowing through the tree moves
-- the selection away and the open file has to stay findable.
treeMark = 3

-- | The only corner in the window, and the scrollbar thumb's own. It goes on
-- the things a pointer grabs or picks and on nothing else; the caret, the
-- selection bands and the rules are all square.
treeRadius :: Float
treeRadius = 3

-- | Where the panel's header sets the root's name: the column a row's icon
-- starts in, so that the name of the folder lines up with what is under it
-- rather than sitting against the panel's edge.
treeHeaderPad :: Float
treeHeaderPad = treePad + treeChevron + 2

-- | A row is scanned rather than read, so it sits tighter than a line of text.
rowHeight :: FontMetrics -> Float
rowHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 2

-- | The panel's header: the root's name, with the header's own padding above
-- and below it. The pane grid's drag picks the pane by this strip.
treeHeaderHeight :: FontMetrics -> Float
treeHeaderHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 12

maxScroll :: Int -> Double -> Double
maxScroll rowCount viewRows = max 0 (fromIntegral rowCount - viewRows)

-- | The view and its scrollbar, over so many rows.
scroller :: Rect -> Int -> Double -> Scroller
scroller rect rowCount viewRows =
  Scroller (rectH rect) (viewRows / max 1 (fromIntegral rowCount)) (maxScroll rowCount viewRows)

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------

-- | Everything the drawing reads.
data Scene = Scene
  { scRows :: !(SmallArray Row)
  , scVersion :: !Int
  , scScroll :: !Double
  , scLineH :: !Float
  , scViewRows :: !Double
  , scSelected :: !(Maybe FilePath)
  , scCurrent :: !(Maybe FilePath)
  , scHovered :: !Int
  , scFocused :: !Bool
  , scThumbHot :: !Bool
  }

-- | A number that changes when what the tree draws does. The version stands
-- for the rows, which are worked out only when they change.
sceneKey :: Scene -> Int
sceneKey sc =
  contentHash
    [ scVersion sc
    , round (scScroll sc * 64)
    , hashText (maybe "" T.pack (scSelected sc))
    , hashText (maybe "" T.pack (scCurrent sc))
    , scHovered sc
    , fromEnum (scFocused sc)
    , fromEnum (scThumbHot sc)
    ]

-- | The draw ops of the rows on screen, and of the scrollbar over them when
-- there is more than the view holds.
drawTree :: CustomDrawContext -> Scene -> Rect -> SmallArray DrawOp
drawTree cdc sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect (tcPanel tc) : concatMap rowOps [first .. last'] ++ bar)
  where
    theme = cdcTheme cdc
    tc = treeColors theme
    fm = cdcFont cdc
    lineH = scLineH sc
    rows = scRows sc
    count = sizeofSmallArray rows
    first = max 0 (floor (scScroll sc))
    last' = min (count - 1) (first + ceiling (h / lineH))
    yOff = realToFrac (fromIntegral first - scScroll sc) * lineH
    rowY i = y + yOff + fromIntegral (i - first) * lineH
    textY ry = ry + (lineH - fmLineHeight fm) / 2
    font weight = TextFont 0 FontRegular weight FontStyleNormal DecorationNone
    -- The lane the scrollbar has, which is nothing until there is more to
    -- show than the view holds.
    lane = if fromIntegral count > scViewRows sc then treeBarW else 0
    -- A row's own colours are in 'TreeColors'; the tint on a file's page is
    -- the family of language it would open as.
    rowOps i =
      let r = indexSmallArray rows i
          ry = rowY i
          indent = treePad + fromIntegral (rowDepth r) * treeIndent
          selected = maybe False (equalFilePath (rowPath r)) (scSelected sc)
          -- The file the editor has, which is a path the application made and
          -- not one of ours, so it is matched the way the platform would.
          isCurrent = maybe False (equalFilePath (rowPath r)) (scCurrent sc)
          -- What a pick lands on is inset rather than run from edge to edge,
          -- so the panel keeps a margin down both sides and the mark has
          -- somewhere of its own to sit.
          pick = Rect (x + treeMark + 2) (ry + 1) (max 0 (w - treeMark - 4 - lane)) (lineH - 2)
          backdrop
            | selected = [FillRoundedRect pick treeRadius (if scFocused sc then tcPicked tc else tcPickedAway tc)]
            | scHovered sc == i = [FillRoundedRect pick treeRadius (tcHover tc)]
            | otherwise = []
          -- One rule for each folder this row sits inside, down the middle of
          -- that folder's own chevron. This is what makes the rows a tree
          -- rather than a list of names: at the top level there is one root
          -- and so no rule at all.
          spine =
            [ FillRect (Rect (rule k) ry 1 lineH) (tcSpine tc)
            | k <- [0 .. rowDepth r - 1]
            ]
          rule k = fromIntegral (round (x + treePad + fromIntegral k * treeIndent + treeChevron / 2) :: Int)
          mark = [FillRect (Rect x (ry + 1) treeMark (lineH - 2)) (tcCurrent tc) | isCurrent]
          -- A folder has a chevron pointing along or down; a file has none.
          cy = ry + lineH / 2
          chevron
            | not (rowDir r) = []
            | otherwise =
                let cx = x + indent + treeChevron / 2
                 in [ if rowOpen r
                        then FillTriangle (cx - 5) (cy - 2.5) (cx + 5) (cy - 2.5) cx (cy + 3.5) (tcMuted tc)
                        else FillTriangle (cx - 2.5) (cy - 5) (cx - 2.5) (cy + 5) (cx + 3.5) cy (tcMuted tc)
                    ]
          icon
            | rowDir r = folderIcon (x + indent + treeChevron) cy (if rowOpen r then tcName tc else tcMuted tc)
            | otherwise = fileIcon (x + indent + treeChevron) cy (languageTint theme (rowPath r))
          tx = x + indent + treeChevron + treeIcon
          avail = w - (tx - x) - treePad - lane
          (weight, color)
            | isCurrent = (WeightSemiBold, tcCurrent tc)
            | rowDir r = (WeightSemiBold, if rowOpen r then tcName tc else tcMuted tc)
            | otherwise = (WeightNormal, tcName tc)
       in backdrop ++ spine ++ mark ++ chevron ++ icon ++ [DrawTextStyled tx (textY ry) (font weight) (elide fm avail (rowName r)) color]

    bar
      | lane <= 0 = []
      | otherwise =
          let (thumbTop, thumbH) = thumbSpan (scroller rect count (scViewRows sc)) (scScroll sc)
           in [ FillRoundedRect
                  (Rect (x + w - treeBarW + 2) (y + thumbTop + 2) (treeBarW - 4) (thumbH - 4))
                  3
                  (if scThumbHot sc then tcThumbHot tc else tcThumb tc)
              ]

-- | The icons, built out of the shapes the toolkit has rather than loaded
-- from an image: two of them, because two is all the tree has to say. A
-- folder is a body under a tab. A file is a page with its top corner taken
-- off, which is the shape everyone reads as a document.
--
-- The corner is taken off rather than shaded over: the page is drawn as the
-- five-sided shape it ends up being, so what shows through the cut is
-- whatever the row behind it is, and one icon draws the same over a row that
-- is picked, hovered or plain. A shade would have to know the row's colour,
-- and at eleven pixels it did not read as a fold anyway.
--
-- Both are square-cornered. The one radius in the window belongs to the
-- things a pointer grabs or picks; an icon is neither.
--
-- Both are centred on @cy@, in a column @treeIcon@ wide starting at @ix@.
folderIcon :: Float -> Float -> Color -> [DrawOp]
folderIcon ix cy col =
  [ FillRect (Rect fx (cy - 5) 5 2) col
  , FillRect (Rect fx (cy - 3) 12 8) col
  ]
  where
    fx = fromIntegral (round ix :: Int) + 2

fileIcon :: Float -> Float -> Color -> [DrawOp]
fileIcon ix cy col =
  [ FillRect (Rect fx fy 5 4) col
  , FillRect (Rect fx (fy + 4) 9 8) col
  , FillTriangle (fx + 5) fy (fx + 9) (fy + 4) (fx + 5) (fy + 4) col
  ]
  where
    fx = fromIntegral (round ix :: Int) + 4
    fy = cy - 6

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
