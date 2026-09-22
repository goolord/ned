-- | Characters, and the grid they are drawn on.
--
-- What is here knows nothing of a buffer, a widget or a window: it answers
-- how wide a character is, what kind of character it is, and where a column
-- lands on the grid of cells the editor lays text out in. Every layer above
-- measures text through this one, so the buffer's idea of a column and the
-- view's idea of a cell cannot drift apart.
module Ned.Text
  ( -- * Bounds
    clamp

    -- * The grid
  , tabWidth
  , longLineLimit
  , cellsAt
  , cellOfCol

    -- * The widest line
  , Width
  , widest

    -- * Kinds of character
  , CharClass (..)
  , classOf
  , indentOf

    -- * Comparing
  , foldCase
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NanoRope.Measured (Measure (..))

--------------------------------------------------------------------------------
-- Bounds
--------------------------------------------------------------------------------

-- | A value held within bounds. It sits at the bottom of the editor so that
-- everything above can reach it; it is about text no more than @max@ is.
clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

--------------------------------------------------------------------------------
-- The grid
--------------------------------------------------------------------------------

tabWidth :: Int
tabWidth = 4

-- | Lines longer than this are never read whole: they are drawn and measured
-- a window at a time, with every character one cell wide.
longLineLimit :: Int
longLineLimit = 4096

-- | How many cells of the grid a character takes, tabs aside.
charCells :: Char -> Int
charCells c
  | c < '\x1100' = 1
  | c <= '\x115F' = 2
  | c >= '\x2E80' && c <= '\xA4CF' = 2
  | c >= '\xAC00' && c <= '\xD7A3' = 2
  | c >= '\xF900' && c <= '\xFAFF' = 2
  | c >= '\xFE30' && c <= '\xFE4F' = 2
  | c >= '\xFF00' && c <= '\xFF60' = 2
  | c >= '\xFFE0' && c <= '\xFFE6' = 2
  | c >= '\x1F300' && c <= '\x1FAFF' = 2
  | c >= '\x20000' && c <= '\x3FFFD' = 2
  | otherwise = 1

-- | How many cells a character takes when it starts at a cell: a tab runs to
-- the next tab stop.
cellsAt :: Int -> Char -> Int
cellsAt cell '\t' = tabWidth - cell `rem` tabWidth
cellsAt _ c = charCells c

-- | The cell a column sits at in a line whose text is at hand.
cellOfCol :: Text -> Int -> Int
cellOfCol line col = T.foldl' (\v c -> v + cellsAt v c) 0 (T.take col line)

--------------------------------------------------------------------------------
-- The widest line
--------------------------------------------------------------------------------

-- | The widest line of a piece of text, and what the piece holds of the lines
-- it starts and ends partway through. It is a rope's measure: every node of
-- the buffer keeps the one for the text under it, an edit works out afresh
-- only the nodes on its path, and the widest line of a whole document is
-- read off the root without reading a line.
data Width
  = -- | A piece with no newline in it.
    Within !Stretch
  | -- | A piece across lines: the end of the line it starts in, the widest of
    -- the lines it holds whole, and the start of the line it ends in.
    Across !Stretch !Int !Stretch

-- | Part of a line as the grid lays it out. Where it ends turns on the cell it
-- starts at only as far as its first tab, which runs to a tab stop; from that
-- stop on, every cell is settled.
data Stretch
  = -- | No tab: so many characters, so many cells.
    Flat !Int !Int
  | -- | So many characters, the cells before the first tab, and the cells
    -- after the stop it runs to.
    Tabbed !Int !Int !Int

instance Semigroup Stretch where
  Flat c a <> Flat d b = Flat (c + d) (a + b)
  Flat c a <> Tabbed d b r = Tabbed (c + d) (a + b) r
  Tabbed c a r <> Flat d b = Tabbed (c + d) a (r + b)
  -- The first stretch ends a whole number of stops past a stop, so where the
  -- second's first tab runs to is known without knowing where either began.
  Tabbed c a r <> Tabbed d b s = Tabbed (c + d) a (tabStop (r + b) + s)

instance Semigroup Width where
  Within a <> Within b = Within (a <> b)
  Within a <> Across h w t = Across (a <> h) w t
  Across h w t <> Within b = Across h w (t <> b)
  Across h w t <> Across h' w' t' = Across h (max w (max (lineCells (t <> h')) w')) t'

instance Monoid Width where
  mempty = Within (Flat 0 0)

-- | The semigroup's sum of a chunk's characters, spelt out as one pass with
-- the line it is in the middle of held in registers: every character of a
-- file goes through here as the file is read.
instance Measure Width where
  measureChunk t = case T.foldl' step (Scan Nothing 0 0 0 none) t of
    Scan Nothing _ c a r -> Within (settle c a r)
    Scan (Just first) w c a r -> Across first w (settle c a r)
    where
      step (Scan first w c a r) ch
        | ch == '\n' = case first of
            Nothing -> Scan (Just (settle c a r)) w 0 0 none
            Just _ -> Scan first (max w (lineCells (settle c a r))) 0 0 none
        | ch == '\t' = Scan first w (c + 1) a (if r == none then 0 else tabStop r)
        | r == none = Scan first w (c + 1) (a + charCells ch) r
        | otherwise = Scan first w (c + 1) a (r + charCells ch)
      settle c a r = if r == none then Flat c a else Tabbed c a r
      -- The cells after the first tab's stop, while there has been no tab.
      none = -1

-- | A chunk as far as it has been measured: the end of the line it starts in
-- once a newline has been passed, the widest line held whole since, and the
-- characters, cells and cells past a tab of the line it is in the middle of.
data Scan = Scan !(Maybe Stretch) !Int !Int !Int !Int

-- | The widest line, in cells.
widest :: Width -> Int
widest = \case
  Within s -> lineCells s
  Across h w t -> max (lineCells h) (max w (lineCells t))

-- | The cells of a whole line: a long one is a cell a character, as the view
-- draws it.
lineCells :: Stretch -> Int
lineCells = \case
  Flat c a -> if c > longLineLimit then c else a
  Tabbed c a r -> if c > longLineLimit then c else tabStop a + r

-- | The stop a tab at a cell runs to.
tabStop :: Int -> Int
tabStop cell = cell + cellsAt cell '\t'

--------------------------------------------------------------------------------
-- Kinds of character
--------------------------------------------------------------------------------

-- | What a character counts as when a word boundary is being looked for. A
-- run of one class is a word, and the boundaries are where the class turns
-- over.
data CharClass = ClassSpace | ClassWord | ClassPunct
  deriving (Eq)

classOf :: Char -> CharClass
classOf c
  | isSpace c = ClassSpace
  | isAlphaNum c || c == '_' || c == '\'' = ClassWord
  | otherwise = ClassPunct

-- | The spaces and tabs a line starts with.
indentOf :: Text -> Text
indentOf = T.takeWhile (\c -> c == ' ' || c == '\t')

--------------------------------------------------------------------------------
-- Comparing
--------------------------------------------------------------------------------

-- | Fold ASCII letters to lower case. It keeps the length of a text, so
-- offsets into the folded text are offsets into the text.
foldCase :: Text -> Text
foldCase = T.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)
