-- | The text being edited: a rope, a cursor with a selection anchor, and an
-- undo history.
--
-- Every location is an offset in code points ('Chars'), which the rope turns
-- into lines and columns in logarithmic time, so nothing here reads more of
-- the document than the line or the window of text it works on. A rope is
-- persistent and an edit shares all but a path with the rope before it, so
-- the undo history keeps whole ropes and costs a few hundred bytes an entry.
--
-- How wide a character is and what class it belongs to are "Ned.Text"'s to
-- say; this module turns those answers into the lines, columns and cells of
-- a document.
module Ned.Buffer
  ( -- * Buffers
    Buffer
  , empty
  , fromText
  , bufRope
  , bufCursor
  , bufAnchor
  , bufVersion
  , isDirty
  , markSaved
  , usesTabs
  , setUsesTabs

    -- * Geometry
  , longLineLimit
  , lineCount
  , lineOf
  , lineStart
  , lineLength
  , lineText
  , lineWindow
  , isLongLine
  , cursorPosition
  , colToVisual
  , visualToCol
  , offsetAt

    -- * Selection
  , hasSelection
  , selectionRange
  , selectedText
  , selectAll
  , selectWordAt
  , wordRangeAt
  , selectWordsFrom
  , selectLineAt
  , selectLines
  , setCursor

    -- * Movement
  , moveLeft
  , moveRight
  , moveUp
  , moveDown
  , moveLines
  , moveHome
  , moveEnd
  , moveWordLeft
  , moveWordRight
  , moveDocStart
  , moveDocEnd
  , gotoLine

    -- * Editing
  , insertText
  , newline
  , backspace
  , deleteForward
  , deleteWordBack
  , deleteWordForward
  , deleteSelection
  , indentKey
  , unindentKey
  , replaceRange

    -- * History
  , undo
  , redo
  , canUndo
  , canRedo

    -- * Search
  , findNext
  , findPrev
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.NanoRope (Position (..), Rope, Unit (..))
import qualified Data.Text.NanoRope as Rope
import Ned.Text (cellOfCol, cellsAt, clamp, classOf, foldCase, indentOf, tabWidth)

--------------------------------------------------------------------------------
-- Buffers
--------------------------------------------------------------------------------

data Buffer = Buffer
  { bufRope :: !Rope
  , bufCursor :: !Int
  -- ^ Offset of the caret, in code points.
  , bufAnchor :: !Int
  -- ^ The other end of the selection; the caret when nothing is selected.
  , bufPrefCol :: !Int
  -- ^ The visual column vertical movement aims for, or -1 when the caret's
  -- own column is it.
  , bufVersion :: !Int
  -- ^ Names the text: two buffers of one history with the same version hold
  -- the same text. Undo brings a version back along with its text.
  , bufNextVersion :: !Int
  , bufSavedVersion :: !Int
  , bufUndo :: ![Snapshot]
  , bufUndoDepth :: !Int
  , bufRedo :: ![Snapshot]
  , bufLastEdit :: !EditKind
  , bufLastEnd :: !Int
  -- ^ Where the last edit left off, for telling whether the next continues it.
  , bufTabs :: !Bool
  -- ^ Whether the Tab key inserts a tab and not spaces.
  }

data Snapshot = Snapshot
  { snapRope :: !Rope
  , snapCursor :: !Int
  , snapAnchor :: !Int
  , snapVersion :: !Int
  }

-- | Edits of one kind that continue each other undo together.
data EditKind = EditOther | EditType | EditSpace | EditBackspace | EditDelete
  deriving (Eq)

-- | Whether an edit of a kind goes on with the run before it. Typing goes on
-- through the spaces before a word and stops at the spaces after it, so a
-- step of the history is a word and the spaces leading up to it.
continuesRun :: EditKind -> EditKind -> Bool
continuesRun _ EditOther = False
continuesRun before EditType = before == EditType || before == EditSpace
continuesRun before kind = before == kind

empty :: Buffer
empty = fromText T.empty

-- | A buffer over a text, which should have its line endings as @\\n@. It
-- indents with tabs when the text does.
fromText :: Text -> Buffer
fromText t =
  Buffer
    { bufRope = Rope.fromText t
    , bufCursor = 0
    , bufAnchor = 0
    , bufPrefCol = -1
    , bufVersion = 0
    , bufNextVersion = 1
    , bufSavedVersion = 0
    , bufUndo = []
    , bufUndoDepth = 0
    , bufRedo = []
    , bufLastEdit = EditOther
    , bufLastEnd = 0
    , bufTabs = any (T.isPrefixOf "\t") (take 2000 (T.lines (T.take 200000 t)))
    }

isDirty :: Buffer -> Bool
isDirty b = bufVersion b /= bufSavedVersion b

markSaved :: Buffer -> Buffer
markSaved b = b {bufSavedVersion = bufVersion b, bufLastEdit = EditOther}

usesTabs :: Buffer -> Bool
usesTabs = bufTabs

setUsesTabs :: Bool -> Buffer -> Buffer
setUsesTabs t b = b {bufTabs = t}

--------------------------------------------------------------------------------
-- Geometry
--------------------------------------------------------------------------------

-- | Lines longer than this are never read whole: they are drawn and measured
-- a window at a time, with every character one cell wide.
longLineLimit :: Int
longLineLimit = 4096

size :: Buffer -> Int
size = Rope.length Chars . bufRope

lineCount :: Buffer -> Int
lineCount = Rope.lineCount . bufRope

-- | The line holding an offset.
lineOf :: Buffer -> Int -> Int
lineOf b off = Rope.convert Chars Lines off (bufRope b)

-- | The offset a line starts at; the end of the text for a line past the last.
lineStart :: Buffer -> Int -> Int
lineStart b ln
  | ln <= 0 = 0
  | ln >= lineCount b = size b
  | otherwise = Rope.convert Lines Chars ln (bufRope b)

-- | Length of a line without its newline.
lineLength :: Buffer -> Int -> Int
lineLength b ln
  | ln < 0 || ln >= n = 0
  | ln + 1 < n = lineStart b (ln + 1) - 1 - lineStart b ln
  | otherwise = size b - lineStart b ln
  where
    n = lineCount b

isLongLine :: Buffer -> Int -> Bool
isLongLine b ln = lineLength b ln > longLineLimit

-- | A whole line, without its newline.
lineText :: Buffer -> Int -> Text
lineText b ln = lineWindow b ln 0 maxBound

-- | The characters of a line from column @c0@ up to column @c1@.
lineWindow :: Buffer -> Int -> Int -> Int -> Text
lineWindow b ln c0 c1 =
  let s = lineStart b ln
      len = lineLength b ln
      from = clamp 0 len c0
      to = clamp from len c1
   in if to <= from then T.empty else Rope.sliceText Chars (s + from) (s + to) (bufRope b)

-- | Line and column of the caret, both from zero, the column in code points.
cursorPosition :: Buffer -> (Int, Int)
cursorPosition b =
  let Position ln col = Rope.offsetToPosition Chars Chars (bufCursor b) (bufRope b)
   in (ln, col)

-- | The cell a column of a line sits at.
colToVisual :: Buffer -> Int -> Int -> Int
colToVisual b ln col
  | isLongLine b ln = col
  | otherwise = cellOfCol (lineWindow b ln 0 col) col

-- | The column of a line nearest a cell.
visualToCol :: Buffer -> Int -> Int -> Int
visualToCol b ln vis
  | isLongLine b ln = clamp 0 (lineLength b ln) vis
  | otherwise = go 0 0 (lineText b ln)
  where
    go !col !v t = case T.uncons t of
      Nothing -> col
      Just (c, rest) ->
        let w = cellsAt v c
         in if vis * 2 < v * 2 + w then col else go (col + 1) (v + w) rest

-- | The offset of a line and a cell, both clamped to the text.
offsetAt :: Buffer -> Int -> Int -> Int
offsetAt b ln vis =
  let ln' = clamp 0 (lineCount b - 1) ln
   in lineStart b ln' + visualToCol b ln' vis

--------------------------------------------------------------------------------
-- Selection
--------------------------------------------------------------------------------

hasSelection :: Buffer -> Bool
hasSelection b = bufCursor b /= bufAnchor b

selectionRange :: Buffer -> (Int, Int)
selectionRange b = (min (bufCursor b) (bufAnchor b), max (bufCursor b) (bufAnchor b))

selectedText :: Buffer -> Text
selectedText b =
  let (i, j) = selectionRange b
   in if i == j then T.empty else Rope.sliceText Chars i j (bufRope b)

selectAll :: Buffer -> Buffer
selectAll b = moved b {bufAnchor = 0, bufCursor = size b}

-- | Put the caret at an offset, keeping the anchor when extending.
setCursor :: Bool -> Int -> Buffer -> Buffer
setCursor extend off b =
  let off' = clamp 0 (size b) off
   in moved b {bufCursor = off', bufAnchor = if extend then bufAnchor b else off'}

-- | A movement ends the run of edits that undo together.
moved :: Buffer -> Buffer
moved b = b {bufPrefCol = -1, bufLastEdit = EditOther}

-- | How far a scan for a word boundary reads.
scanWindow :: Int
scanWindow = 1024

textBefore, textAfter :: Buffer -> Int -> Text
textBefore b off = Rope.sliceText Chars (max 0 (off - scanWindow)) off (bufRope b)
textAfter b off = Rope.sliceText Chars off (min (size b) (off + scanWindow)) (bufRope b)

-- | The word, the run of spaces or the run of punctuation at an offset, as
-- the offsets it starts and ends at; nothing at all on an empty line.
wordRangeAt :: Int -> Buffer -> (Int, Int)
wordRangeAt off0 b =
  let off = clamp 0 (size b) off0
      after = textAfter b off
      before = textBefore b off
      cls = case T.uncons after of
        Just (c, _) | c /= '\n' -> Just (classOf c)
        _ -> case T.unsnoc before of
          Just (_, c) | c /= '\n' -> Just (classOf c)
          _ -> Nothing
   in case cls of
        Nothing -> (off, off)
        Just k ->
          let same c = c /= '\n' && classOf c == k
           in (off - T.length (T.takeWhileEnd same before), off + T.length (T.takeWhile same after))

-- | Select the word at an offset.
selectWordAt :: Int -> Buffer -> Buffer
selectWordAt off b =
  let (i, j) = wordRangeAt off b
   in moved b {bufAnchor = i, bufCursor = j}

-- | Select from a word out to the word at an offset, whole words at both
-- ends: what dragging on from a double click does. The caret is on the side
-- of the offset.
selectWordsFrom :: (Int, Int) -> Int -> Buffer -> Buffer
selectWordsFrom (i0, j0) off b =
  let (i, j) = wordRangeAt off b
   in if off < i0
        then moved b {bufAnchor = j0, bufCursor = min i i0}
        else moved b {bufAnchor = i0, bufCursor = max j j0}

-- | Select a line and its newline.
selectLineAt :: Int -> Buffer -> Buffer
selectLineAt off b =
  let ln = lineOf b off
   in moved b {bufAnchor = lineStart b ln, bufCursor = lineStart b (ln + 1)}

-- | Select whole lines from one line to another, the caret on the side of
-- the second: after it when it is at or below the first, and before it when
-- it is above.
selectLines :: Int -> Int -> Buffer -> Buffer
selectLines from to b
  | to >= from = moved b {bufAnchor = lineStart b from, bufCursor = lineStart b (to + 1)}
  | otherwise = moved b {bufAnchor = lineStart b (from + 1), bufCursor = lineStart b to}

--------------------------------------------------------------------------------
-- Movement
--------------------------------------------------------------------------------

moveLeft, moveRight :: Bool -> Buffer -> Buffer
moveLeft extend b
  | not extend && hasSelection b = setCursor False (fst (selectionRange b)) b
  | otherwise = setCursor extend (bufCursor b - 1) b
moveRight extend b
  | not extend && hasSelection b = setCursor False (snd (selectionRange b)) b
  | otherwise = setCursor extend (bufCursor b + 1) b

moveUp, moveDown :: Bool -> Buffer -> Buffer
moveUp = moveLines (-1)
moveDown = moveLines 1

-- | Move the caret by lines, holding on to the cell it started from.
moveLines :: Int -> Bool -> Buffer -> Buffer
moveLines n extend b =
  let (ln, col) = cursorPosition b
      vis = if bufPrefCol b >= 0 then bufPrefCol b else colToVisual b ln col
      target = ln + n
      off
        | target < 0 = 0
        | target >= lineCount b = size b
        | otherwise = offsetAt b target vis
   in (setCursor extend off b) {bufPrefCol = vis}

-- | To the first character of the line that is not a space, or to the start
-- of the line from there.
moveHome :: Bool -> Buffer -> Buffer
moveHome extend b =
  let (ln, col) = cursorPosition b
      indent = T.length (indentOf (lineWindow b ln 0 longLineLimit))
      col' = if col == indent then 0 else indent
   in setCursor extend (lineStart b ln + col') b

moveEnd :: Bool -> Buffer -> Buffer
moveEnd extend b =
  let (ln, _) = cursorPosition b
   in setCursor extend (lineStart b ln + lineLength b ln) b

wordLeftOf, wordRightOf :: Buffer -> Int -> Int
wordLeftOf b off =
  let before = textBefore b off
      spaces = T.takeWhileEnd isSpace before
      rest = T.dropEnd (T.length spaces) before
      run = case T.unsnoc rest of
        Nothing -> T.empty
        Just (_, c) -> T.takeWhileEnd (\x -> classOf x == classOf c) rest
   in off - T.length spaces - T.length run
wordRightOf b off =
  let after = textAfter b off
      run = case T.uncons after of
        Nothing -> T.empty
        Just (c, _)
          | isSpace c -> T.empty
          | otherwise -> T.takeWhile (\x -> classOf x == classOf c) after
      spaces = T.takeWhile isSpace (T.drop (T.length run) after)
   in off + T.length run + T.length spaces

moveWordLeft, moveWordRight :: Bool -> Buffer -> Buffer
moveWordLeft extend b = setCursor extend (wordLeftOf b (bufCursor b)) b
moveWordRight extend b = setCursor extend (wordRightOf b (bufCursor b)) b

moveDocStart, moveDocEnd :: Bool -> Buffer -> Buffer
moveDocStart extend = setCursor extend 0
moveDocEnd extend b = setCursor extend (size b) b

-- | To the start of a line, counted from one.
gotoLine :: Int -> Buffer -> Buffer
gotoLine n b = setCursor False (lineStart b (clamp 0 (lineCount b - 1) (n - 1))) b

--------------------------------------------------------------------------------
-- Editing
--------------------------------------------------------------------------------

undoLimit :: Int
undoLimit = 2000

-- | Replace the text from @i@ up to @j@ and leave the caret after it. Every
-- change to the text comes through here.
edit :: EditKind -> Int -> Int -> Text -> Buffer -> Buffer
edit kind i j t b
  | i >= j && T.null t = b
  | otherwise = commit continues kind rope' end end b
  where
    rope'
      | i >= j = Rope.insert Chars i t (bufRope b)
      | T.null t = Rope.delete Chars i j (bufRope b)
      | otherwise = Rope.replace Chars i j t (bufRope b)
    end = i + T.length t
    continues =
      continuesRun (bufLastEdit b) kind
        && not (hasSelection b)
        && bufLastEnd b == (if kind == EditBackspace then j else i)

-- | Put a new text in place, with its anchor and caret, as a step of the
-- history: the text before it goes on the undo stack, unless this edit
-- continues the run before it and undoes with that.
commit :: Bool -> EditKind -> Rope -> Int -> Int -> Buffer -> Buffer
commit continues kind rope anchor cursor b =
  b
    { bufRope = rope
    , bufCursor = cursor
    , bufAnchor = anchor
    , bufPrefCol = -1
    , bufVersion = bufNextVersion b
    , bufNextVersion = bufNextVersion b + 1
    , bufUndo = undos
    , bufUndoDepth = depth
    , bufRedo = []
    , bufLastEdit = kind
    , bufLastEnd = cursor
    }
  where
    (undos, depth)
      | continues = (bufUndo b, bufUndoDepth b)
      | bufUndoDepth b >= 2 * undoLimit = (current b : take undoLimit (bufUndo b), undoLimit + 1)
      | otherwise = (current b : bufUndo b, bufUndoDepth b + 1)

-- | Replace a range, as one step of the history.
replaceRange :: Int -> Int -> Text -> Buffer -> Buffer
replaceRange = edit EditOther

-- | Type or paste a text over the selection. Line endings become @\\n@. No
-- text is nothing done: the selection stays, and is not deleted.
insertText :: Text -> Buffer -> Buffer
insertText t0 b
  | T.null t = b
  | otherwise = edit kind i j t b
  where
    t = T.filter (/= '\r') t0
    (i, j) = selectionRange b
    kind
      | i /= j || T.length t /= 1 || t == "\n" = EditOther
      | t == " " = EditSpace
      | otherwise = EditType

-- | Break the line, carrying its indentation over.
newline :: Buffer -> Buffer
newline b =
  let (i, j) = selectionRange b
      ln = lineOf b i
      col = i - lineStart b ln
      indent = indentOf (lineWindow b ln 0 (min col longLineLimit))
   in edit EditOther i j (T.cons '\n' indent) b

deleteSelection :: Buffer -> Buffer
deleteSelection b =
  let (i, j) = selectionRange b
   in edit EditOther i j T.empty b

-- | Delete the selection, or the character before the caret. Within
-- indentation made of spaces that is back to the previous tab stop.
backspace :: Buffer -> Buffer
backspace b
  | hasSelection b = deleteSelection b
  | cur == 0 = b
  | otherwise =
      let (ln, col) = cursorPosition b
          lead = lineWindow b ln 0 (min col longLineLimit)
          n
            | col > 0 && col <= longLineLimit && T.all (== ' ') lead = ((col - 1) `rem` tabWidth) + 1
            | otherwise = 1
       in edit EditBackspace (cur - n) cur T.empty b
  where
    cur = bufCursor b

deleteForward :: Buffer -> Buffer
deleteForward b
  | hasSelection b = deleteSelection b
  | otherwise = edit EditDelete (bufCursor b) (min (size b) (bufCursor b + 1)) T.empty b

deleteWordBack, deleteWordForward :: Buffer -> Buffer
deleteWordBack b
  | hasSelection b = deleteSelection b
  | otherwise = edit EditOther (wordLeftOf b (bufCursor b)) (bufCursor b) T.empty b
deleteWordForward b
  | hasSelection b = deleteSelection b
  | otherwise = edit EditOther (bufCursor b) (wordRightOf b (bufCursor b)) T.empty b

indentUnit :: Buffer -> Text
indentUnit b = if bufTabs b then "\t" else T.replicate tabWidth " "

-- | The lines a selection touches. A selection ending at the start of a line
-- leaves that line out.
selectedLines :: Buffer -> (Int, Int)
selectedLines b =
  let (i, j) = selectionRange b
      l0 = lineOf b i
      l1 = lineOf b j
   in (l0, if l1 > l0 && j == lineStart b l1 then l1 - 1 else l1)

-- | Tab: indent the lines of a selection that spans lines, and otherwise
-- type up to the next tab stop.
indentKey :: Buffer -> Buffer
indentKey b
  | l1 > l0 = reindent (\_ -> Just (0, 0, indentUnit b)) b
  | bufTabs b = insertText "\t" b
  | otherwise =
      let (i, j) = selectionRange b
          ln = lineOf b i
          vis = colToVisual b ln (i - lineStart b ln)
       in edit EditOther i j (T.replicate (cellsAt vis '\t') " ") b
  where
    (l0, l1) = selectedLines b

-- | Shift+Tab: take one step of indentation off the selected lines.
unindentKey :: Buffer -> Buffer
unindentKey = reindent $ \lead ->
  case T.uncons lead of
    Just ('\t', _) -> Just (0, 1, T.empty)
    Just (' ', _) -> Just (0, T.length (T.takeWhile (== ' ') (T.take tabWidth lead)), T.empty)
    _ -> Nothing

-- | Edit the start of each selected line: given the head of a line, the
-- columns to replace and the text to put there. A selection grows to whole
-- lines, and the change is one step of the history.
reindent :: (Text -> Maybe (Int, Int, Text)) -> Buffer -> Buffer
reindent f b =
  let (l0, l1) = selectedLines b
      step rope ln =
        let s = Rope.convert Lines Chars ln rope
            lead = Rope.sliceText Chars s (s + tabWidth) rope
         in case f (T.takeWhile (/= '\n') lead) of
              Nothing -> rope
              Just (c0, c1, t) -> Rope.replace Chars (s + c0) (s + c1) t rope
      -- From the last line up, so that the offsets of earlier lines hold.
      rope' = foldl' step (bufRope b) [l1, l1 - 1 .. l0]
      changed = Rope.length Chars rope' /= size b
      start = Rope.convert Lines Chars l0 rope'
      end
        | l1 + 1 < Rope.lineCount rope' = Rope.convert Lines Chars (l1 + 1) rope'
        | otherwise = Rope.length Chars rope'
      -- A bare caret stays a caret, moved with its line's text; a selection
      -- grows to the lines it touched.
      caret = max start (bufCursor b + Rope.length Chars rope' - size b)
      (anchor', cursor')
        | hasSelection b = (start, end)
        | otherwise = (caret, caret)
   in if changed then commit False EditOther rope' anchor' cursor' b else b

--------------------------------------------------------------------------------
-- History
--------------------------------------------------------------------------------

canUndo, canRedo :: Buffer -> Bool
canUndo = not . null . bufUndo
canRedo = not . null . bufRedo

restore :: Snapshot -> Buffer -> Buffer
restore s b =
  b
    { bufRope = snapRope s
    , bufCursor = snapCursor s
    , bufAnchor = snapAnchor s
    , bufVersion = snapVersion s
    , bufPrefCol = -1
    , bufLastEdit = EditOther
    }

current :: Buffer -> Snapshot
current b = Snapshot (bufRope b) (bufCursor b) (bufAnchor b) (bufVersion b)

undo :: Buffer -> Buffer
undo b = case bufUndo b of
  [] -> b
  s : rest ->
    (restore s b) {bufUndo = rest, bufUndoDepth = bufUndoDepth b - 1, bufRedo = current b : bufRedo b}

redo :: Buffer -> Buffer
redo b = case bufRedo b of
  [] -> b
  s : rest ->
    (restore s b) {bufRedo = rest, bufUndo = current b : bufUndo b, bufUndoDepth = bufUndoDepth b + 1}

--------------------------------------------------------------------------------
-- Search
--------------------------------------------------------------------------------

-- | How much of the rope a search reads at a time, given the needle's
-- length. A window steps on by its length less the needle's, so it has to be
-- the longer of the two by a margin for the search to get anywhere.
searchWindowFor :: Int -> Int
searchWindowFor n = max 65536 (2 * n)

-- | The first match at or after an offset, going around the end of the text.
-- The needle is matched as it is when the flag is set, and without regard to
-- ASCII case otherwise.
findFrom :: Bool -> Text -> Int -> Buffer -> Maybe Int
findFrom exact needle0 from b
  | T.null needle0 = Nothing
  | otherwise = case scan from total of
      Just i -> Just i
      Nothing -> scan 0 (min total (from + n - 1))
  where
    needle = if exact then needle0 else foldCase needle0
    n = T.length needle
    window = searchWindowFor n
    total = size b
    rope = bufRope b
    -- Windows overlap by the needle less one, so a match across a seam is
    -- found in the next window.
    scan !at !end
      | at + n > end = Nothing
      | otherwise =
          let to = min end (at + window)
              w0 = Rope.sliceText Chars at to rope
              w = if exact then w0 else foldCase w0
              (pre, match) = T.breakOn needle w
           in if not (T.null match)
                then Just (at + T.length pre)
                else if to >= end then Nothing else scan (to - n + 1) end

-- | The last match ending at or before an offset, going around the start.
findBack :: Bool -> Text -> Int -> Buffer -> Maybe Int
findBack exact needle0 before b
  | T.null needle0 = Nothing
  | otherwise = case scan before 0 of
      Just i -> Just i
      Nothing -> scan total (max 0 (before - n + 1))
  where
    needle = if exact then needle0 else foldCase needle0
    n = T.length needle
    window = searchWindowFor n
    total = size b
    rope = bufRope b
    scan !end !start
      | end - n < start = Nothing
      | otherwise =
          let from = max start (end - window)
              w0 = Rope.sliceText Chars from end rope
              w = if exact then w0 else foldCase w0
              -- Everything up to the end of the last match, or nothing.
              (pre, _) = T.breakOnEnd needle w
           in if not (T.null pre)
                then Just (from + T.length pre - n)
                else if from <= start then Nothing else scan (from + n - 1) start

-- | Select the next match after the selection.
findNext :: Bool -> Text -> Buffer -> Maybe Buffer
findNext exact needle b =
  let from = if hasSelection b then fst (selectionRange b) + 1 else bufCursor b
   in select needle <$> findFrom exact needle (min (size b) from) b <*> pure b

-- | Select the previous match before the selection.
findPrev :: Bool -> Text -> Buffer -> Maybe Buffer
findPrev exact needle b =
  let before = if hasSelection b then snd (selectionRange b) - 1 else bufCursor b
   in select needle <$> findBack exact needle (max 0 before) b <*> pure b

select :: Text -> Int -> Buffer -> Buffer
select needle i b = moved b {bufAnchor = i, bufCursor = i + T.length needle}
