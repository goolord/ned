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
  , cellsAt
  , cellOfCol

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
