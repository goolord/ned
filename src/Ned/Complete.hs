-- | Completing the word before the caret: where the words come from, which
-- of them answer what has been typed, and the menu of them that Tab steps
-- through.
--
-- A 'Source' is a function from what has been typed to the words that start
-- with it, best first, and nothing more: the text being edited, the other
-- files open, a tags file, the language's keywords. Each is a lazy list, so
-- a source is read only as far as the menu wants it, and one further down
-- the order is not read at all while those above it have enough to offer.
--
-- Nothing here draws, and nothing reads a key. What Tab does is in
-- "Ned.Editor.Keys", and the menu as it looks is in "Ned.View.Editor".
module Ned.Complete
  ( -- * Where words come from
    Candidate (..)
  , Source
  , bufferSource
  , buffersSource
  , keywordSource
  , gather
  , answers

    -- * Words
  , identChar
  , wordsIn
  , wordBefore

    -- * The menu
  , Completion (..)
  , openCompletion
  , stepCompletion
  , cancelCompletion
  , settleCompletion
  , menuLimit
  ) where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isDigit, isUpper)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Highlight (Lang (..))

--------------------------------------------------------------------------------
-- Where words come from
--------------------------------------------------------------------------------

-- | A word the menu offers, and where it is from.
data Candidate = Candidate
  { candWord :: !Text
  , candNote :: !Text
  -- ^ What the menu says beside the word: the file it is from, or what kind
  -- of word it is. A word of the text being edited says nothing.
  }
  deriving (Eq, Show)

-- | The words that answer what has been typed ('answers'), best first. The
-- same word may come more than once; 'gather' keeps the first.
type Source = Text -> [Candidate]

-- | Whether a word answers what has been typed: it is longer, and starts
-- with it. What is typed in lower case matches any case; a capital in it
-- asks for the case as typed.
answers :: Text -> Text -> Bool
answers typed w = T.length w > T.length typed && startsWith typed w

-- | 'answers', short of asking the word to be any longer.
startsWith :: Text -> Text -> Bool
startsWith typed w
  | T.any isUpper typed = typed `T.isPrefixOf` w
  | otherwise = typed `T.isPrefixOf` T.toLower (T.take (T.length typed) w)

-- | How many lines either side of the caret the text being edited is read,
-- and how many lines from the top of each other file are.
nearLines, farLines :: Int
nearLines = 5000
farLines = 20000

-- | The words of the text being edited, the nearest the caret first: the
-- caret's own line out from the caret, then the lines round it, one above
-- and one below at a time. The word the caret is in is not offered, since it
-- is the one being completed.
bufferSource :: Lang -> Buffer -> Source
bufferSource lang b typed =
  [ Candidate w T.empty
  | ln <- outward
  , not (B.isLongLine b ln)
  , (col, w) <- lineWords ln
  , not (ln == cl && col <= cc && cc <= col + T.length w)
  , answers typed w
  ]
  where
    (cl, cc) = B.cursorPosition b
    n = B.lineCount b
    outward = cl : concat [[l | l <- [cl - d, cl + d], l >= 0, l < n] | d <- [1 .. min nearLines (max cl (n - cl))]]
    lineWords ln
      | ln == cl = sortOn (\(col, _) -> abs (col - cc)) (wordsIn lang (B.lineText b ln))
      | otherwise = wordsIn lang (B.lineText b ln)

-- | The words of other files, each under its name, in the order given and
-- from the top of each. Each is read as its own language reads a word.
buffersSource :: [(Text, Lang, Buffer)] -> Source
buffersSource docs typed =
  [ Candidate w name
  | (name, lang, b) <- docs
  , ln <- [0 .. min farLines (B.lineCount b) - 1]
  , not (B.isLongLine b ln)
  , (_, w) <- wordsIn lang (B.lineText b ln)
  , answers typed w
  ]

-- | The words a language reserves, and the types it names.
keywordSource :: Lang -> Source
keywordSource lang typed =
  [Candidate w "keyword" | w <- Set.toAscList (langKeywords lang), answers typed w]
    <> [Candidate w "type" | w <- Set.toAscList (langTypes lang), answers typed w]

-- | The words a source offers for what has been typed, each once and at the
-- first place it was offered, as many as a menu keeps.
gather :: Source -> Text -> [Candidate]
gather source typed = take poolLimit (go Set.empty (source typed))
  where
    go _ [] = []
    go seen (c : cs)
      | Set.member (candWord c) seen = go seen cs
      | otherwise = c : go (Set.insert (candWord c) seen) cs

-- | How many words a menu keeps. A letter or two answers thousands, and
-- nobody steps through them all.
poolLimit :: Int
poolLimit = 500

--------------------------------------------------------------------------------
-- Words
--------------------------------------------------------------------------------

-- | Whether a character is one of a language's identifiers.
identChar :: Lang -> Char -> Bool
identChar lang c = isAlphaNum c || c == '_' || c `elem` langIdentExtra lang

-- | The words of a line and the column each starts at. A run that starts
-- with a digit is a number and no word, and a quote before a word, which a
-- language that allows one in a name allows there too, is not part of it.
wordsIn :: Lang -> Text -> [(Int, Text)]
wordsIn lang = go 0
  where
    ident = identChar lang
    go !col t
      | T.null t = []
      | otherwise =
          let (gap, rest) = T.span (not . ident) t
              (run, rest') = T.span ident rest
              at = col + T.length gap
              quotes = T.length (T.takeWhile (== '\'') run)
              w = T.drop quotes run
              next = go (at + T.length run) rest'
           in case T.uncons w of
                Just (h, _) | not (isDigit h) -> (at + quotes, w) : next
                _ -> next

-- | The word the caret is at the end of, and the offset it starts at.
wordBefore :: Lang -> Buffer -> Maybe (Int, Text)
wordBefore lang b = case wordsIn lang run of
  [(col, w)] | col + T.length w == T.length run -> Just (B.bufCursor b - T.length w, w)
  _ -> Nothing
  where
    (ln, col0) = B.cursorPosition b
    run = T.takeWhileEnd (identChar lang) (B.lineWindow b ln (max 0 (col0 - 256)) col0)

--------------------------------------------------------------------------------
-- The menu
--------------------------------------------------------------------------------

-- | A menu of words for the one before the caret. The word in the text is
-- always the one picked, or what was typed while none is.
data Completion = Completion
  { cmStart :: !Int
  -- ^ Where the word being completed starts.
  , cmOrigin :: !Text
  -- ^ What had been typed of it when the menu opened, which the pool answers.
  , cmTyped :: !Text
  -- ^ What has been typed of it since, which cancelling puts back.
  , cmPool :: !(V.Vector Candidate)
  -- ^ Every word that answered the origin; typing on narrows it.
  , cmShown :: !(V.Vector Candidate)
  -- ^ The words that answer what is typed now, in the menu's order.
  , cmPicked :: !Int
  -- ^ The word in the text, or -1 while that is what was typed.
  }

-- | How many rows of the menu show at once.
menuLimit :: Int
menuLimit = 10

-- | Complete the word before the caret: the first word the source offers
-- goes in its place, and the menu opens on it. When only one word answers,
-- it goes in with no menu. Nothing is done, and 'Nothing' comes back, when
-- there is no word before the caret or nothing answers it.
openCompletion :: Source -> Lang -> Buffer -> Maybe (Maybe Completion, Buffer)
openCompletion source lang b = do
  guard (not (B.hasSelection b))
  (start, typed) <- wordBefore lang b
  let pool = V.fromList (gather source typed)
      menu = Completion start typed typed pool pool (-1)
  guard (not (V.null pool))
  let (menu', b') = stepCompletion 1 menu b
  pure (if V.length pool == 1 then Nothing else Just menu', b')

-- | Pick the word so many rows down the menu, or up it, and put it in the
-- text. Past either end is what was typed, and then round to the other end.
stepCompletion :: Int -> Completion -> Buffer -> (Completion, Buffer)
stepCompletion by m b = (m {cmPicked = picked}, B.completeWord (cmStart m) (B.bufCursor b) word b)
  where
    n = V.length (cmShown m)
    picked = ((cmPicked m + 1 + by) `mod` (n + 1)) - 1
    word = if picked < 0 then cmTyped m else candWord (cmShown m V.! picked)

-- | Put back what was typed, for the menu to close on.
cancelCompletion :: Completion -> Buffer -> Buffer
cancelCompletion m b
  | cmPicked m < 0 = b
  | otherwise = B.completeWord (cmStart m) (B.bufCursor b) (cmTyped m) b

-- | The menu after something other than the menu changed the text, from how
-- it was before to how it is. Typing on at the end of the word narrows it,
-- and so does taking some of it back, down to what it opened on; what does
-- anything else to the text, or moves the caret, closes it.
settleCompletion :: Lang -> Completion -> Buffer -> Buffer -> Maybe Completion
settleCompletion lang m before after
  | B.bufVersion after == B.bufVersion before = m <$ guard (B.bufCursor after == B.bufCursor before)
  | otherwise = do
      guard (not (B.hasSelection after))
      (start, typed) <- wordBefore lang after
      guard (start == cmStart m && startsWith (cmOrigin m) typed)
      let shown = V.filter (answers typed . candWord) (cmPool m)
      guard (not (V.null shown))
      pure m {cmTyped = typed, cmShown = shown, cmPicked = -1}
