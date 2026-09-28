-- | Vim's modes over the editor: normal and visual mode, the operators and
-- motions their commands are made of, and the command line.
--
-- It is a layer over "Ned.Editor.Keys" rather than beside it. Insert mode is
-- the editor as it is without vim, and every other mode turns keys into what
-- the buffer knows how to do already: moves, selections, edits. What is the
-- application's -- find a file, save, close -- goes back as a 'Request' for
-- it to answer, since the editor knows of no files.
--
-- Past 'vimKeys' this is pure. The keys arrive as the characters a terminal
-- would send (Escape as @\\ESC@, Ctrl+R as @\\DC2@) and the clipboard is
-- handed in, so the tests run it with no window.
module Ned.Editor.Vim
  ( Vim (..)
  , Mode (..)
  , Request (..)
  , Clip (..)
  , newVim
  , vimKeys
  , feedKeys
  , vimSettle
  , vimBlock
  , vimLabel
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, void)
import Data.Char (isDigit, isSpace, isUpper, toLower, toUpper)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.NanoRope.Measured as Rope
import NanoUI (Input (..), Key (..), Modifiers (..), NanoUI, foldInputKeys, getClipboard, inputKeysElem, setClipboard)
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor.Keys (applyKeys)
import Ned.Text (clamp, classOf, indentOf, longLineLimit)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- The state
--------------------------------------------------------------------------------

data Mode = Normal | Insert | Visual | VisualLine
  deriving (Eq, Show)

data Vim = Vim
  { vimMode :: !Mode
  , vimAnchor :: !Int
  -- ^ In a visual mode, the end of the selection that stays where it is.
  , vimCursor :: !Int
  -- ^ In a visual mode, the character the caret is on. The buffer's own
  -- caret is past it, or at a line's start, since what the buffer selects
  -- runs up to its caret and not through it.
  , vimPending :: !Text
  -- ^ The keys of a command not finished yet, or the command line after its
  -- @:@.
  , vimLastFind :: !(Maybe (Char, Char))
  -- ^ The last of @f@, @F@, @t@ and @T@ and what it looked for, which @;@ and
  -- @,@ look for again.
  , vimRequests :: ![Request]
  -- ^ What the application has been asked to do, oldest first.
  , vimHeld :: !Int
  -- ^ The frames in a row that j or k has come round again in while held,
  -- which is how much faster it goes.
  }

-- | What the keys ask of the application.
data Request
  = FindFile
  | Grep
  | ToggleTree
  | FindBar
  | -- | The next match of what the find bar holds, or the previous one.
    FindAgain !Bool
  | Save
  | -- | Close the tab, or quit with the last one; with the flag, whatever
    -- would be lost.
    Quit !Bool
  | QuitAll !Bool
  | NextTab !Bool
  | Message !Text
  deriving (Eq, Show)

-- | Reading and writing the clipboard, which is the register every yank
-- and put goes through.
data Clip m = Clip
  { clipGet :: m (Maybe Text)
  , clipSet :: Text -> m ()
  }

newVim :: Vim
newVim = Vim Normal 0 0 T.empty Nothing [] 0

-- | What the status bar calls the mode.
vimLabel :: Vim -> Text
vimLabel v = case vimMode v of
  Normal -> "NORMAL"
  Insert -> "INSERT"
  Visual -> "VISUAL"
  VisualLine -> "V-LINE"

-- | The character the block caret is on, in the modes that draw one.
vimBlock :: Vim -> Buffer -> Maybe Int
vimBlock v b = case vimMode v of
  Insert -> Nothing
  Normal -> Just (B.bufCursor b)
  _ -> Just (min (B.size b) (vimCursor v))

-- | The leader's bindings, after the space that starts them.
leaderKeys :: [(String, Request)]
leaderKeys =
  [ ("ff", FindFile)
  , ("fg", Grep)
  , ("d", ToggleTree)
  ]

--------------------------------------------------------------------------------
-- The keys
--------------------------------------------------------------------------------

-- | Run the frame's keys through vim. Insert mode is the editor's own keys
-- less Escape, and Ctrl+W, which deletes the word before the caret; the
-- other modes read what was typed, and the named keys are the motions they
-- stand for.
vimKeys :: Input -> Int -> Vim -> Buffer -> NanoUI (Vim, Buffer)
vimKeys inp page v b = case vimMode v of
  Insert
    | KeyEscape `elem` keys -> applyKeys inp page b >>= \b' -> feed "\ESC" b'
    | modCtrl mods && not (modAlt mods) && KeyChar 'w' `elem` keys -> feed "\ETB" b
    | otherwise -> (v,) <$> applyKeys inp page b
  _
    | modCtrl mods && not (modAlt mods) -> case traverse control [c | KeyChar c <- keys] of
        Just cs@(_ : _) -> feed cs b
        -- The editor's own chords, the clipboard's and select all's.
        _ -> vimSettle True v <$> applyKeys inp page b
    | otherwise ->
        feedKeys clip page (concatMap accelerate (mapMaybe named keys ++ T.unpack (inputChars inp))) (v {vimHeld = held}, b)
  where
    mods = inputModifiers inp
    keys = reverse (foldInputKeys (flip (:)) [] (inputKeys inp))
    clip = Clip getClipboard (void . setClipboard)
    feed ks = feedKeys clip page ks . (v,)
    -- Held down, j and k go further with each repeat, as accelerated-jk
    -- has them: a line more a repeat at each of these counts of repeats.
    -- A frame with no keys in it is between two repeats, and changes nothing.
    held
      | any repeating [KeyChar 'j', KeyChar 'k', KeyDown, KeyUp] = vimHeld v + 1
      | null keys = vimHeld v
      | otherwise = 0
    repeating k = inputKeysElem k (inputKeys inp) && not (inputKeysElem k (inputKeysNew inp))
    stride = 1 + length (takeWhile (<= held) [7, 12, 17, 21, 24, 26, 28, 30 :: Int])
    accelerate c
      | (c == 'j' || c == 'k') && stride > 1 && T.null (vimPending v) = show stride ++ [c]
      | otherwise = [c]
    control = \case
      'n' -> Just '\SO'
      'p' -> Just '\DLE'
      'r' -> Just '\DC2'
      'd' -> Just '\EOT'
      'u' -> Just '\NAK'
      _ -> Nothing
    commandLine = ":" `T.isPrefixOf` vimPending v
    named = \case
      KeyEscape -> Just '\ESC'
      KeyEnter -> Just '\r'
      KeyBackspace -> Just '\b'
      _ | commandLine -> Nothing
      KeyDelete -> Just '\DEL'
      KeyLeft -> Just 'h'
      KeyRight -> Just 'l'
      KeyUp | modAlt mods -> Just '\STX'
      KeyDown | modAlt mods -> Just '\ACK'
      KeyUp -> Just 'k'
      KeyDown -> Just 'j'
      KeyHome -> Just '0'
      KeyEnd -> Just '$'
      KeyPageUp -> Just '\STX'
      KeyPageDown -> Just '\ACK'
      _ -> Nothing

-- | Run keys, one after the other; @page@ is how many lines a page is.
feedKeys :: Monad m => Clip m -> Int -> String -> (Vim, Buffer) -> m (Vim, Buffer)
feedKeys clip page ks st = foldM (flip (key clip page)) st ks

key :: Monad m => Clip m -> Int -> Char -> (Vim, Buffer) -> m (Vim, Buffer)
key clip page c (v, b) = case vimMode v of
  Insert -> pure $ case c of
    '\ESC' -> (v {vimMode = Normal}, stepBack b)
    '\r' -> (v, B.newline b)
    '\b' -> (v, B.backspace b)
    '\DEL' -> (v, B.deleteForward b)
    '\ETB' -> (v, B.deleteWordBack b)
    _ | c >= ' ' -> (v, B.insertText (T.singleton c) b)
    _ -> (v, b)
  _ | Just (':', typed) <- T.uncons pending -> pure $ case c of
    '\ESC' -> (done, b)
    '\r' -> tidy (ex typed done b)
    '\b' -> (v {vimPending = T.dropEnd 1 pending}, b)
    _ | c >= ' ' -> (v {vimPending = T.snoc pending c}, b)
    _ -> (v, b)
  _ | c == '\ESC' -> pure . tidy $ case vimMode v of
    _ | not (T.null pending) -> (done, b)
    Normal -> (v, b)
    _ -> leaveVisual v b
  _ -> case command clip page v (T.unpack keys) of
    More -> pure (v {vimPending = keys}, b)
    Bad -> pure (done, b)
    Got step -> tidy <$> step done b
  where
    pending = vimPending v
    keys = T.snoc pending c
    done = v {vimPending = T.empty}
    -- Leaving insert mode puts the caret on the character before it.
    stepBack buf =
      let (_, col) = B.cursorPosition buf
       in B.setCursor False (if col > 0 then B.bufCursor buf - 1 else B.bufCursor buf) buf

-- | After a command: a visual mode's selection shown in the buffer, and
-- normal mode's caret on a character.
tidy :: (Vim, Buffer) -> (Vim, Buffer)
tidy (v, b) = case vimMode v of
  Insert -> (v, b)
  -- A selection left by something else, a find's match or an undo, gives
  -- way to its start.
  Normal -> (v, onChar (if B.hasSelection b then B.setCursor False (fst (B.selectionRange b)) b else b))
  -- Off the empty line after a last line break, as normal mode's is.
  _ ->
    let v' = v {vimCursor = min (vimCursor v) (lineEnd b (lastLine b))}
     in (v', shown v' b)

-- | Bring vim into line with what was done to the buffer from outside it.
-- A selection the pointer made (@picked@) is visual mode; any other, such as
-- the match a find selects, leaves the caret at its start in normal mode, as
-- vim's search does. A caret put past the end of a line is stepped back onto
-- it.
vimSettle :: Bool -> Vim -> Buffer -> (Vim, Buffer)
vimSettle picked v b = case vimMode v of
  Insert -> (v, b)
  _ | visual && same (shown v b) -> (v, b)
  _ | B.hasSelection b && picked ->
        let v' = v {vimMode = Visual, vimAnchor = if c > a then a else a - 1, vimCursor = if c > a then c - 1 else c}
         in (v', shown v' b)
  _ -> (v {vimMode = Normal}, onChar (B.setCursor False (min a c) b))
  where
    a = B.bufAnchor b
    c = B.bufCursor b
    visual = vimMode v == Visual || vimMode v == VisualLine
    same b' = B.bufAnchor b' == a && B.bufCursor b' == c

-- | Back to normal mode from a visual one, the caret where it was.
leaveVisual :: Vim -> Buffer -> (Vim, Buffer)
leaveVisual v b = (v {vimMode = Normal}, B.setCursor False (vimCursor v) b)

-- | The buffer selecting what a visual mode has: through the character the
-- caret is on, or the whole of the lines.
shown :: Vim -> Buffer -> Buffer
shown v b = case vimMode v of
  VisualLine
    | lc >= la -> B.placeCaret (B.lineStart b la) (B.lineStart b (lc + 1)) b
    | otherwise -> B.placeCaret (B.lineStart b (la + 1)) (B.lineStart b lc) b
  _
    | c >= a -> B.placeCaret a (c + 1) b
    | otherwise -> B.placeCaret (a + 1) c b
  where
    a = vimAnchor v
    c = vimCursor v
    la = B.lineOf b a
    lc = B.lineOf b c

-- | Step a caret past the end of its line back onto the last character, and
-- off the empty line after a text's last line break, which vim has no line
-- for.
onChar :: Buffer -> Buffer
onChar b
  | ln > lastLine b = onChar (B.placeCaret (B.lineStart b (lastLine b)) (B.lineStart b (lastLine b)) b)
  | len > 0 && col >= len = B.placeCaret end end b
  | otherwise = b
  where
    (ln, col) = B.cursorPosition b
    len = B.lineLength b ln
    end = B.lineStart b ln + len - 1

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | Where a command is after its keys so far.
data P a = More | Bad | Got a
  deriving (Functor)

-- | The first of two readings of the keys that has them.
orElse :: P a -> P a -> P a
orElse (Got a) _ = Got a
orElse Bad q = q
orElse More (Got a) = Got a
orElse More _ = More

type Step m = Vim -> Buffer -> m (Vim, Buffer)

-- | What a command's keys, its count first, ask for.
command :: Monad m => Clip m -> Int -> Vim -> String -> P (Step m)
command clip page v s0 = case vimMode v of
  Normal -> case s of
    [] -> More
    o : rest | o `elem` ("dcy<>" :: String) -> operator o rest
    ":" | Nothing <- count -> More
    _ -> normalAction `orElse` requests `orElse` (moving <$> motion page v s)
  _ -> case s of
    [] -> More
    _ -> visualAction `orElse` requests `orElse` (object <$> textObject s) `orElse` (moving <$> motion page v s)
  where
    (count, s) = counted s0
    n = fromMaybe 1 count
    pureStep f = Got (\v' b -> pure (f v' b))
    ask rs = pureStep (\v' b -> (request rs v', b))
    enter m = pureStep (\v' b -> let c = B.bufCursor b in (v' {vimMode = m, vimAnchor = c, vimCursor = c}, b))
    insertAt f = pureStep (\v' b -> (v' {vimMode = Insert}, B.setCursor False (f b) b))

    normalAction = case s of
      "i" -> insertAt B.bufCursor
      "a" -> insertAt (\b -> min (lineEnd b (line b)) (B.bufCursor b + 1))
      "I" -> insertAt (\b -> B.firstNonBlank b (line b))
      "A" -> insertAt (B.bufCursor . B.moveEnd False)
      "o" -> pureStep (\v' b -> (v' {vimMode = Insert}, B.newline (B.moveEnd False b)))
      "O" -> pureStep $ \v' b ->
        let st = B.lineStart b (line b)
            indent = indentOf (lineHead b (line b))
         in (v' {vimMode = Insert}, B.setCursor False (st + T.length indent) (B.replace st st (indent <> "\n") b))
      [k] | k == 'x' || k == '\DEL' -> operator 'd' "l"
      "X" -> operator 'd' "h"
      "s" -> operator 'c' "l"
      "S" -> operator 'c' "c"
      "D" -> operator 'd' "$"
      "C" -> operator 'c' "$"
      "Y" -> operator 'y' "y"
      [k] | k == 'p' || k == 'P' -> Got (paste clip (k == 'p') n)
      "u" -> pureStep (\v' b -> (v', times n B.undo b))
      "\DC2" -> pureStep (\v' b -> (v', times n B.redo b))
      "J" -> pureStep (\v' b -> (v', times (max 1 (n - 1)) joinLine b))
      "r" -> More
      ['r', ch] | ch >= ' ' -> pureStep (\v' b -> (v', replaceChars ch n b))
      "~" -> pureStep (\v' b -> (v', toggleCase n b))
      "v" -> enter Visual
      "V" -> enter VisualLine
      _ -> Bad

    visualAction = case s of
      _ | Just (o, whole) <- lookup s visualOps -> Got $ \v' b ->
            let sp = visualSpan v' b
             in operate clip o (if whole then uncurry Lines (spanLines b sp) else sp) (min (vimAnchor v') (vimCursor v')) v' b
      [k] | k == 'p' || k == 'P' -> Got (putOver clip)
      "o" -> pureStep (\v' b -> (v' {vimAnchor = vimCursor v', vimCursor = vimAnchor v'}, b))
      "v" -> switch Visual
      "V" -> switch VisualLine
      "J" -> pureStep $ \v' b ->
        let (l1, l2) = spanLines b (visualSpan v' b)
         in (v' {vimMode = Normal}, times (max 1 (l2 - l1)) joinLine (B.setCursor False (B.lineStart b l1) b))
      "~" -> recase (T.map swapCase)
      "u" -> recase T.toLower
      "U" -> recase T.toUpper
      _ -> Bad

    visualOps =
      [ ("d", ('d', False)), ("x", ('d', False)), ("\DEL", ('d', False)), ("D", ('d', True)), ("X", ('d', True))
      , ("y", ('y', False)), ("Y", ('y', True))
      , ("c", ('c', False)), ("s", ('c', False)), ("C", ('c', True)), ("S", ('c', True))
      , (">", ('>', False)), ("<", ('<', False))
      ]

    switch m = pureStep $ \v' b ->
      if vimMode v' == m then leaveVisual v' b else (v' {vimMode = m}, b)

    recase f = pureStep $ \v' b ->
      let (i, j) = spanRange b (visualSpan v' b)
       in (v' {vimMode = Normal}, B.setCursor False i (B.replace i j (f (slice b i j)) b))

    requests = case s of
      "n" -> ask [FindAgain True]
      "N" -> ask [FindAgain False]
      [k] | k == '/' || k == '?' -> ask [FindBar]
      "g" -> More
      "gt" -> ask [NextTab True]
      "gT" -> ask [NextTab False]
      "Z" -> More
      "ZZ" -> ask [Save, Quit False]
      "ZQ" -> ask [Quit True]
      ' ' : ks
        | Just r <- lookup ks leaderKeys -> ask [r]
        | any ((ks `isPrefixOf`) . fst) leaderKeys -> More
      _ -> Bad

    -- A motion moves the caret, and in a visual mode the end of the
    -- selection it is on.
    moving m v' b = pure $
      let p = position v' b
       in case mGo m count (B.placeCaret p p b) of
            Nothing -> (v', b)
            Just b' ->
              let v'' = remember m v'
               in if vimMode v' == Normal then (v'', b') else (v'' {vimCursor = B.bufCursor b'}, b')

    -- A text object in a visual mode selects it.
    object f v' b = pure $ case f (B.setCursor False (vimCursor v') b) of
      Just (i, j) | j > i -> (v' {vimMode = Visual, vimAnchor = i, vimCursor = j - 1}, b)
      _ -> (v', b)

    -- An operator, and then what it works on: its own key again for whole
    -- lines, a text object, or a motion. A count before either multiplies.
    operator o rest = case s' of
      [] -> More
      [x] | x == o -> Got $ \v' b ->
        let l = line b
         in operate clip o (Lines l (max l (clampLine b (l + total - 1)))) (B.bufCursor b) v' b
      _ -> (onObject <$> textObject s') `orElse` (onMotion <$> motion page v s')
      where
        (count2, s') = counted rest
        both = case (count, count2) of
          (Nothing, Nothing) -> Nothing
          _ -> Just (n * fromMaybe 1 count2)
        total = fromMaybe 1 both
        onObject f v' b = case f b of
          Just (i, j) -> operate clip o (Chars i j) i v' b
          Nothing -> pure (v', b)
        onMotion m v' b =
          -- cw changes to the end of the word, and not the spaces after it.
          let m' = if o == 'c' && s' == "w" && not (isSpace (charAt b c)) then changeWord else m
              c = B.bufCursor b
           in case mGo m' both b of
                Nothing -> pure (v', b)
                Just b' ->
                  let t = B.bufCursor b'
                      sp = case mKind m' of
                        Linewise -> Lines (B.lineOf b (min c t)) (clampLine b (B.lineOf b (max c t)))
                        Inclusive -> Chars (min c t) (min (B.size b) (max c t + 1))
                        Exclusive -> exclusiveSpan (s' == "w") b (min c t) (max c t)
                   in operate clip o sp (min c t) (remember m' v') b

-- | Ask the application for something.
request :: [Request] -> Vim -> Vim
request rs v = v {vimRequests = vimRequests v ++ rs}

-- | Keep a find for ; and , to repeat.
remember :: Motion -> Vim -> Vim
remember m v = v {vimLastFind = mFind m <|> vimLastFind v}

-- | A count before a command, and the rest of it. A 0 on its own is a motion.
counted :: String -> (Maybe Int, String)
counted s = case span isDigit s of
  (ds@(d : _), rest) | d /= '0' -> (Just (min 99999 (fromMaybe 1 (readMaybe ds))), rest)
  _ -> (Nothing, s)

-- | Where the caret is: in a visual mode, the character it is on.
position :: Vim -> Buffer -> Int
position v b = if vimMode v == Normal then B.bufCursor b else vimCursor v

-- | A command line after its @:@.
ex :: Text -> Vim -> Buffer -> (Vim, Buffer)
ex cmd v b = case T.unpack (T.strip cmd) of
  "" -> (v, b)
  "w" -> ask [Save]
  "q" -> ask [Quit False]
  "q!" -> ask [Quit True]
  "wq" -> ask [Save, Quit False]
  "x" -> ask [Save, Quit False]
  "qa" -> ask [QuitAll False]
  "qa!" -> ask [QuitAll True]
  other
    | Just ln <- readMaybe other -> (v, B.setCursor False (B.firstNonBlank b (clampLine b (ln - 1))) b)
    | otherwise -> ask [Message ("Not an editor command: " <> T.pack other)]
  where
    ask rs = (request rs v, b)

--------------------------------------------------------------------------------
-- Motions
--------------------------------------------------------------------------------

-- | How an operator takes what a motion passes over: up to where it lands,
-- through it, or the whole of every line it touches.
data Kind = Exclusive | Inclusive | Linewise
  deriving (Eq)

data Motion = Motion
  { mKind :: !Kind
  , mFind :: !(Maybe (Char, Char))
  -- ^ The find it is, for @;@ to repeat.
  , mGo :: Maybe Int -> Buffer -> Maybe Buffer
  -- ^ Move the caret, given the count, if it can go.
  }

motion :: Int -> Vim -> String -> P Motion
motion page v = \case
  "h" -> left
  "\b" -> left
  "l" -> exclusive (\n b -> to b (min (lineEnd b (line b)) (B.bufCursor b + n)))
  -- As far as the last line or the first, and nowhere from there.
  [k] | k == 'j' || k == '\SO' -> linewise (\n b -> if line b >= lastLine b then Nothing else Just (B.moveLines (min n (lastLine b - line b)) False b))
  [k] | k == 'k' || k == '\DLE' -> linewise (\n b -> if line b <= 0 then Nothing else Just (B.moveLines (negate (min n (line b))) False b))
  [k] | k == '+' || k == '\r' -> linewise (\n b -> nonBlank b (line b + n))
  "-" -> linewise (\n b -> nonBlank b (line b - n))
  "w" -> exclusive (\n b -> Just (times n (B.moveWordRight False) b))
  "b" -> exclusive (\n b -> Just (times n (B.moveWordLeft False) b))
  "e" -> Got (Motion Inclusive Nothing (\cnt -> Just . times (fromMaybe 1 cnt) wordEnd))
  "0" -> exclusive (\_ b -> to b (B.lineStart b (line b)))
  "^" -> exclusive (\_ b -> to b (B.firstNonBlank b (line b)))
  "$" -> exclusive (\n b -> to b (lineEnd b (clampLine b (line b + n - 1))))
  "g" -> More
  "gg" -> Got (Motion Linewise Nothing (\cnt b -> nonBlank b (maybe 0 (subtract 1) cnt)))
  "G" -> Got (Motion Linewise Nothing (\cnt b -> nonBlank b (maybe (lastLine b) (subtract 1) cnt)))
  "}" -> exclusive (\n b -> Just (times n (\b' -> B.setCursor False (paragraph True b') b') b))
  "{" -> exclusive (\n b -> Just (times n (\b' -> B.setCursor False (paragraph False b') b') b))
  [k] | k `elem` ("fFtT" :: String) -> More
  [k, ch] | k `elem` ("fFtT" :: String) -> Got (findChar k ch)
  ";" -> maybe Bad (\(k, ch) -> Got (findChar k ch) {mFind = Nothing}) (vimLastFind v)
  "," -> maybe Bad (\(k, ch) -> Got (findChar (reverseFind k) ch) {mFind = Nothing}) (vimLastFind v)
  "\EOT" -> scroll (max 1 (page `div` 2))
  "\NAK" -> scroll (negate (max 1 (page `div` 2)))
  "\ACK" -> scroll page
  "\STX" -> scroll (negate page)
  _ -> Bad
  where
    scroll by = linewise (\n b -> Just (B.moveLines (n * by) False b))
    left = exclusive (\n b -> to b (max (B.lineStart b (line b)) (B.bufCursor b - n)))
    exclusive f = Got (Motion Exclusive Nothing (f . fromMaybe 1))
    linewise f = Got (Motion Linewise Nothing (f . fromMaybe 1))
    reverseFind = \case 'f' -> 'F'; 'F' -> 'f'; 't' -> 'T'; _ -> 't'

to :: Buffer -> Int -> Maybe Buffer
to b off = Just (B.setCursor False off b)

nonBlank :: Buffer -> Int -> Maybe Buffer
nonBlank b ln = to b (B.firstNonBlank b (clampLine b ln))

-- | The end of the word the caret is in, for cw: e, but from the caret
-- itself, so that it does not go on to the next word from a word's end.
changeWord :: Motion
changeWord = Motion Inclusive Nothing $ \cnt b ->
  Just (times (fromMaybe 1 cnt - 1) wordEnd (B.setCursor False (fromMaybe (B.bufCursor b) (endOfWord b (B.bufCursor b))) b))

-- | e: to the end of the next word.
wordEnd :: Buffer -> Buffer
wordEnd b = maybe b (\e -> B.setCursor False e b) (endOfWord b (B.bufCursor b + 1))

-- | The last character of the word at or after an offset, if there is one.
endOfWord :: Buffer -> Int -> Maybe Int
endOfWord b off =
  let t = B.textAfter b off
      spaces = T.length (T.takeWhile isSpace t)
      rest = T.drop spaces t
   in case T.uncons rest of
        Nothing -> Nothing
        Just (x, _) -> Just (off + spaces + T.length (T.takeWhile ((== classOf x) . classOf) rest) - 1)

-- | The start of the next empty line after the paragraph the caret is in, or
-- of the one before it. Each line is read as where it starts and where the
-- one after it does, a rope query a line.
paragraph :: Bool -> Buffer -> Int
paragraph down b =
  case filter blank (dropWhile blank lns) of
    (_, st, _) : _ -> st
    [] -> if down then B.size b else 0
  where
    l = line b
    n = B.lineCount b
    starts = map (B.lineStart b)
    -- A line and where it and the line after it start; the last line has no
    -- line break, and the start of the line after it is the end of the text.
    blank (i, st, next) = if i == n - 1 then next == st else next == st + 1
    lns
      | down = let ss = starts [l + 1 .. n] in zip3 [l + 1 .. n - 1] ss (drop 1 ss)
      | otherwise = let ss = starts [l, l - 1 .. 0] in zip3 [l - 1, l - 2 .. 0] (drop 1 ss) ss

-- | f, F, t or T: to a character on the line, or next to it.
findChar :: Char -> Char -> Motion
findChar k ch = Motion (if k == 'f' || k == 't' then Inclusive else Exclusive) (Just (k, ch)) $ \cnt b ->
  let n = fromMaybe 1 cnt
      st = B.lineStart b (line b)
      col = B.bufCursor b - st
      found = [i | (i, x) <- zip [0 ..] (T.unpack (lineHead b (line b))), x == ch]
      pick xs = case drop (n - 1) xs of
        i : _ -> Just i
        [] -> Nothing
   in case k of
        'f' -> to b . (st +) =<< pick (filter (> col) found)
        't' -> to b . (\i -> st + i - 1) =<< pick (filter (> col + 1) found)
        'F' -> to b . (st +) =<< pick (reverse (filter (< col) found))
        _ -> to b . (\i -> st + i + 1) =<< pick (reverse (filter (< col - 1) found))

-- | What an exclusive motion's operator takes. w over a line's last word
-- stops at the end of that line, and does not take the line break it only
-- arrived after. Any other that ends at the start of a line does the same,
-- unless it started in its line's indentation, when it takes the lines it
-- passed over whole.
exclusiveSpan :: Bool -> Buffer -> Int -> Int -> Span
exclusiveSpan byWord b from end
  | le > ls && not byWord && atStart && from <= B.firstNonBlank b ls = Lines ls (le - 1)
  | le > ls && (byWord || atStart) = Chars from (max from (lineEnd b (le - 1)))
  | otherwise = Chars from end
  where
    ls = B.lineOf b from
    le = B.lineOf b end
    atStart = end == B.lineStart b le

--------------------------------------------------------------------------------
-- Text objects
--------------------------------------------------------------------------------

-- | iw, aw, and a bracket's or a quote's inside or all of it: where it starts
-- and ends around the caret.
textObject :: String -> P (Buffer -> Maybe (Int, Int))
textObject = \case
  [k] | k == 'i' || k == 'a' -> More
  [k, o]
    | k == 'i' || k == 'a' -> case o of
        'w' -> Got (Just . word (k == 'a'))
        _ | Just (open, close) <- lookup o brackets -> Got (bracket (k == 'a') open close)
        _ | o `elem` ("\"'`" :: String) -> Got (quoted (k == 'a') o)
        _ -> Bad
  _ -> Bad
  where
    brackets =
      [ (c, p)
      | p@(open, close) <- [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
      , c <- [open, close]
      ]
        ++ [('b', ('(', ')')), ('B', ('{', '}'))]

-- | The word at the caret, and with aw the spaces after it, or before it
-- when there are none after.
word :: Bool -> Buffer -> (Int, Int)
word around b
  | not around = (i, j)
  | trailing > 0 = (i, j + trailing)
  | otherwise = (i - leading, j)
  where
    (i, j) = B.wordRangeAt (B.bufCursor b) b
    blank c = c == ' ' || c == '\t'
    trailing = T.length (T.takeWhile blank (B.textAfter b j))
    leading = T.length (T.takeWhileEnd blank (B.textBefore b i))

-- | The brackets around the caret, the one it is on among them. Inside a
-- block whose brackets end and start lines, the lines between are what is
-- inside it.
bracket :: Bool -> Char -> Char -> Buffer -> Maybe (Int, Int)
bracket around open close b = do
  o <- outward (0 :: Int) c (slice b (c + 1 - reach) (c + 1))
  e <- closing (0 :: Int) (o + 1) (slice b (o + 1) (o + 1 + reach))
  pure $
    if around
      then (o, e + 1)
      else
        let i = if charAt b (o + 1) == '\n' then o + 2 else o + 1
            le = B.lineOf b e
            j = if le > B.lineOf b i && T.all isSpace (slice b (B.lineStart b le) e) then B.lineStart b le else e
         in (i, max i j)
  where
    c = min (B.bufCursor b) (B.size b - 1)
    -- Back to the bracket that opens what the caret is in, past the pairs
    -- it is not in, from the end of the text before it.
    outward !d !p t = case T.unsnoc t of
      Nothing -> Nothing
      Just (rest, x)
        | x == open -> if d == 0 then Just p else outward (d - 1) (p - 1) rest
        | x == close && p /= c -> outward (d + 1) (p - 1) rest
        | otherwise -> outward d (p - 1) rest
    -- On to the bracket that closes it.
    closing !d !p t = case T.uncons t of
      Nothing -> Nothing
      Just (x, rest)
        | x == close -> if d == 0 then Just p else closing (d - 1) (p + 1) rest
        | x == open -> closing (d + 1) (p + 1) rest
        | otherwise -> closing d (p + 1) rest

-- | The quotes around the caret on its line, or the next pair after it.
quoted :: Bool -> Char -> Buffer -> Maybe (Int, Int)
quoted around q b =
  case [(p, p') | (p, p') <- pairs quotes, p' >= col] of
    (p, p') : _ -> Just (if around then (st + p, st + p' + 1) else (st + p + 1, st + p'))
    [] -> Nothing
  where
    st = B.lineStart b (line b)
    col = B.bufCursor b - st
    t = T.unpack (lineHead b (line b))
    quotes = [i | (i, x, prev) <- zip3 [0 :: Int ..] t ('\0' : t), x == q, prev /= '\\']
    pairs (x : y : rest) = (x, y) : pairs rest
    pairs _ = []

--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------

-- | What an operator works on: from one offset up to another, or whole lines.
data Span = Chars !Int !Int | Lines !Int !Int

-- | What a visual mode has selected.
visualSpan :: Vim -> Buffer -> Span
visualSpan v b = case vimMode v of
  VisualLine -> Lines (clampLine b (B.lineOf b (min a c))) (clampLine b (B.lineOf b (max a c)))
  _ -> Chars (min a c) (min (B.size b) (max a c + 1))
  where
    a = vimAnchor v
    c = vimCursor v

spanRange :: Buffer -> Span -> (Int, Int)
spanRange b = \case
  Chars i j -> (i, j)
  Lines l1 l2 -> (B.lineStart b l1, B.lineStart b (l2 + 1))

spanLines :: Buffer -> Span -> (Int, Int)
spanLines b = \case
  Chars i j -> (B.lineOf b i, B.lineOf b (max i (j - 1)))
  Lines l1 l2 -> (l1, l2)

-- | Run an operator over a span: d, c and y yank what it covers, and > and <
-- shift its lines. A yank leaves the caret at @land@ when it is of lines.
operate :: Monad m => Clip m -> Char -> Span -> Int -> Vim -> Buffer -> m (Vim, Buffer)
operate clip o sp land v b = case o of
  'y' -> do
    clipSet clip yanked
    pure (normal, B.setCursor False (case sp of Chars i _ -> i; Lines _ _ -> land) b)
  'd' -> do
    clipSet clip yanked
    pure . (normal,) $ case sp of
      Chars i j -> B.replace i j T.empty b
      Lines l1 l2 ->
        -- The last lines take the line break before them with them.
        let (i, j)
              | l2 >= B.lineCount b - 1 && l1 > 0 = (B.lineStart b l1 - 1, B.size b)
              | otherwise = (B.lineStart b l1, B.lineStart b (l2 + 1))
            b' = B.replace i j T.empty b
         in B.setCursor False (B.firstNonBlank b' (clampLine b' l1)) b'
  'c' -> do
    clipSet clip yanked
    pure . (v {vimMode = Insert},) $ case sp of
      Chars i j -> B.replace i j T.empty b
      -- Changing lines keeps the first one's indentation.
      Lines l1 l2 -> B.replace (B.lineStart b l1) (lineEnd b l2) (indentOf (lineHead b l1)) b
  _ ->
    let (l1, l2) = spanLines b sp
        st = B.lineStart b l1
        shifted
          | o == '<' = B.unindentKey (B.selectLines l1 l2 b)
          | l2 > l1 = B.indentKey (B.selectLines l1 l2 b)
          | B.lineLength b l1 == 0 = b
          | otherwise = B.replace st st (B.indentUnit b) b
     in pure (normal, B.setCursor False (B.firstNonBlank shifted l1) shifted)
  where
    normal = v {vimMode = Normal}
    yanked = case sp of
      Chars i j -> slice b i j
      Lines l1 l2 ->
        let t = slice b (B.lineStart b l1) (B.lineStart b (l2 + 1))
         in if "\n" `T.isSuffixOf` t then t else t <> "\n"

-- | p and P: put the clipboard after the caret or before it, a count of
-- times. What ends in a line break is whole lines, and goes on the lines
-- below or above.
paste :: Monad m => Clip m -> Bool -> Int -> Step m
paste clip after n v b = do
  got <- clipGet clip
  pure . (v,) $ case T.filter (/= '\r') <$> got of
    Just t | not (T.null t) -> if "\n" `T.isSuffixOf` t then putLines (T.replicate n t) else putChars (T.replicate n t)
    _ -> b
  where
    c = B.bufCursor b
    l = line b
    putLines body =
      let (at, text, first)
            | not after = (B.lineStart b l, body, l)
            | l + 1 < B.lineCount b = (B.lineStart b (l + 1), body, l + 1)
            | otherwise = (B.size b, "\n" <> T.dropEnd 1 body, l + 1)
          b' = B.replace at at text b
       in B.setCursor False (B.firstNonBlank b' first) b'
    putChars body =
      let at = if after && c < lineEnd b l then c + 1 else c
       in B.setCursor False (at + T.length body - 1) (B.replace at at body b)

-- | p in a visual mode: the clipboard in place of the selection.
putOver :: Monad m => Clip m -> Step m
putOver clip v b = do
  got <- clipGet clip
  let sp = visualSpan v b
      (i, j) = spanRange b sp
      normal = v {vimMode = Normal}
  pure $ case T.filter (/= '\r') <$> got of
    Just t | not (T.null t) ->
      let t' = case sp of
            Lines _ _
              | "\n" `T.isSuffixOf` slice b i j -> if "\n" `T.isSuffixOf` t then t else t <> "\n"
              | otherwise -> T.dropWhileEnd (== '\n') t
            Chars _ _ -> t
       in (normal, B.setCursor False i (B.replace i j t' b))
    _ -> (normal, B.setCursor False (vimCursor v) b)

-- | J: the next line onto the end of this one, its indentation for a space.
joinLine :: Buffer -> Buffer
joinLine b
  | l + 1 >= B.lineCount b = b
  | otherwise = B.setCursor False e (B.replace e (B.lineStart b (l + 1) + lead) sep b)
  where
    l = line b
    e = lineEnd b l
    next = lineHead b (l + 1)
    lead = T.length (T.takeWhile isSpace next)
    rest = T.drop lead next
    sep
      | T.null rest || B.lineLength b l == 0 || ")" `T.isPrefixOf` rest || charAt b (e - 1) == ' ' = T.empty
      | otherwise = " "

-- | r: the characters from the caret, a count of them, replaced with one.
replaceChars :: Char -> Int -> Buffer -> Buffer
replaceChars ch n b
  | c + n > lineEnd b (line b) = b
  | otherwise = B.setCursor False (c + n - 1) (B.replace c (c + n) (T.replicate n (T.singleton ch)) b)
  where
    c = B.bufCursor b

-- | ~: the case of the characters from the caret turned over, and the caret
-- past them.
toggleCase :: Int -> Buffer -> Buffer
toggleCase n b
  | k <= 0 = b
  | otherwise = B.setCursor False (c + k) (B.replace c (c + k) (T.map swapCase (slice b c (c + k))) b)
  where
    c = B.bufCursor b
    k = min n (lineEnd b (line b) - c)

swapCase :: Char -> Char
swapCase c = if isUpper c then toLower c else toUpper c

--------------------------------------------------------------------------------
-- The buffer, as vim reads it
--------------------------------------------------------------------------------

line :: Buffer -> Int
line b = B.lineOf b (B.bufCursor b)

lineEnd :: Buffer -> Int -> Int
lineEnd b ln = B.lineStart b ln + B.lineLength b ln

-- | As much of a line as is read whole.
lineHead :: Buffer -> Int -> Text
lineHead b ln = B.lineWindow b ln 0 longLineLimit

clampLine :: Buffer -> Int -> Int
clampLine b = clamp 0 (lastLine b)

-- | The last line, which is not the empty one after a last line break.
lastLine :: Buffer -> Int
lastLine b
  | n > 0 && B.lineLength b n == 0 = n - 1
  | otherwise = n
  where
    n = B.lineCount b - 1

charAt :: Buffer -> Int -> Char
charAt b i = maybe '\n' fst (T.uncons (slice b i (i + 1)))

-- | The text between two offsets, each held to the text.
slice :: Buffer -> Int -> Int -> Text
slice b i j
  | j' <= i' = T.empty
  | otherwise = Rope.sliceText Rope.Chars i' j' (B.bufRope b)
  where
    i' = max 0 i
    j' = min (B.size b) j

-- | How far a bracket is looked for either side of the caret.
reach :: Int
reach = 65536

-- | A function done a count of times.
times :: Int -> (a -> a) -> a -> a
times n f !x
  | n <= 0 = x
  | otherwise = times (n - 1) f (f x)
