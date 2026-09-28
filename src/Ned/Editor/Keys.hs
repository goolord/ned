-- | The keyboard, and the clipboard it reaches for.
--
-- Every key the editor answers to on its own is here: the movements, the
-- edits, and the chords that are the text's rather than the application's.
-- The chords the application owns -- save, open, find -- never reach this
-- module, so there is one place to read to know what a key does to the text.
-- The one exception is the completion menu, which takes Ctrl+N and Ctrl+P
-- for its own while it is open, and with vim's keys always
-- ('completionKeys').
module Ned.Editor.Keys
  ( applyKeys
  , completionKeys
  , clipboardCopy
  , clipboardCut
  , clipboardPaste
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Maybe (isJust)
import qualified Data.Text as T
import NanoUI
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Complete
import Ned.Highlight (Lang)

-- | Run the frame's keys and typed characters on the buffer. Chords the
-- application owns (save, open, find and so on) are left alone.
--
-- A chord types nothing: Ctrl+C is the key 'c' pressed with Ctrl held, and
-- the characters are only what was typed.
applyKeys :: Input -> Int -> Buffer -> NanoUI Buffer
applyKeys inp page buf0
  | ctrl && not alt = foldM (flip chord) buf1 chordKeys
  -- AltGr arrives as Ctrl+Alt, and what it types is typed.
  | otherwise = pure (B.insertText (T.filter (>= ' ') typed) buf1)
  where
    mods = inputModifiers inp
    shift = modShift mods
    ctrl = modCtrl mods
    alt = modAlt mods
    typed = inputChars inp
    buf1 = foldInputKeys key buf0 (inputKeys inp)
    chordKeys = reverse (foldInputKeys (\cs k -> case k of KeyChar c -> c : cs; _ -> cs) [] (inputKeys inp))

    key b = \case
      KeyLeft -> (if ctrl then B.moveWordLeft else B.moveLeft) shift b
      KeyRight -> (if ctrl then B.moveWordRight else B.moveRight) shift b
      KeyUp -> (if alt then B.moveLines (negate page) else B.moveUp) shift b
      KeyDown -> (if alt then B.moveLines page else B.moveDown) shift b
      KeyHome -> (if ctrl then B.moveDocStart else B.moveHome) shift b
      KeyEnd -> (if ctrl then B.moveDocEnd else B.moveEnd) shift b
      KeyBackspace -> (if ctrl then B.deleteWordBack else B.backspace) b
      KeyDelete -> (if ctrl then B.deleteWordForward else B.deleteForward) b
      KeyEnter -> B.newline b
      KeyTab
        | ctrl || alt -> b
        | shift -> B.unindentKey b
        | otherwise -> B.indentKey b
      KeyPageUp | not ctrl -> B.moveLines (negate page) shift b
      KeyPageDown | not ctrl -> B.moveLines page shift b
      KeyEscape -> B.setCursor False (B.bufCursor b) b
      _ -> b

    chord c b = case c of
      'a' -> pure (B.selectAll b)
      'z' | shift -> pure (B.redo b)
      'z' -> pure (B.undo b)
      'y' -> pure (B.redo b)
      'c' -> clipboardCopy b
      'x' -> clipboardCut b
      'v' -> clipboardPaste b
      _ -> pure b

-- | What a key does to the completion menu.
data MenuKey = MenuOpen | MenuStep !Int | MenuAccept | MenuCancel

-- | The keys of the completion menu, and the Tab that opens it on the word
-- before the caret. While it is open, Tab, Down and Ctrl+N step down it,
-- Shift+Tab, Up and Ctrl+P step up it, Enter takes the word picked, and
-- Ctrl+E puts back what was typed. A key held steps once for each repeat.
-- With vim's keys (@vim@), Ctrl+N and Ctrl+P open it as Tab does, as they do
-- in vim's insert mode.
--
-- 'Nothing' when the frame's key is not the menu's, which leaves the keys to
-- the editor's own and the menu to be settled after them: Tab with no word
-- before the caret, or none that answers it, indents as it always has. What
-- was typed in the frame ahead of the key goes in first, as it came.
completionKeys :: Bool -> Input -> (Buffer -> Source) -> Lang -> Maybe Completion -> Buffer -> Maybe (Maybe Completion, Buffer)
completionKeys vim inp sourceFor lang menu0 buf0 = do
  (action, n) <- pressed
  case (menu1, action) of
    (Just m, MenuStep by) -> Just (first Just (stepCompletion (by * n) m buf1))
    (Just m, MenuAccept) | cmPicked m >= 0 -> Just (Nothing, buf1)
    (Just m, MenuCancel) -> Just (Nothing, cancelCompletion m buf1)
    (Nothing, MenuOpen) -> do
      (m, b) <- openCompletion (sourceFor buf1) lang buf1
      pure (maybe (Nothing, b) (\m' -> first Just (stepCompletion (n - 1) m' b)) m)
    _ -> Nothing
  where
    mods = inputModifiers inp
    typed = T.filter (>= ' ') (inputChars inp)
    buf1 = B.insertText typed buf0
    menu1 = if T.null typed then menu0 else menu0 >>= \m -> settleCompletion lang m buf0 buf1
    open = isJust menu1
    -- The frame's one command key, and how many times it came: the keys
    -- before it are the typing.
    pressed = case foldInputKeys (flip (:)) [] (inputKeys inp) of
      k : rest -> (,1 + length (takeWhile (== k) rest)) <$> meaning k
      [] -> Nothing
    only s c a = modShift mods == s && modCtrl mods == c && modAlt mods == a
    meaning = \case
      KeyTab | only False False False -> Just (if open then MenuStep 1 else MenuOpen)
      KeyTab | only True False False, open -> Just (MenuStep (-1))
      KeyDown | only False False False, open -> Just (MenuStep 1)
      KeyUp | only False False False, open -> Just (MenuStep (-1))
      KeyChar c | c == 'n' || c == 'p', only False True False, not open, vim -> Just MenuOpen
      KeyChar 'n' | only False True False, open -> Just (MenuStep 1)
      KeyChar 'p' | only False True False, open -> Just (MenuStep (-1))
      KeyChar 'e' | only False True False, open -> Just MenuCancel
      KeyEnter | only False False False, open -> Just MenuAccept
      _ -> Nothing

-- | Copy, cut and paste through the host's clipboard. With nothing selected,
-- copy and cut take the whole line.
clipboardCopy, clipboardCut, clipboardPaste :: Buffer -> NanoUI Buffer
clipboardCopy b = b <$ copyFrom (orLine b)
clipboardCut b = do
  copied <- copyFrom (orLine b)
  pure (if copied then B.deleteSelection (orLine b) else b)
clipboardPaste b = maybe b (`B.insertText` b) <$> getClipboard

-- | Put the selection on the clipboard, and say whether it went. An empty
-- line has nothing to take, and what the clipboard holds stays.
copyFrom :: Buffer -> NanoUI Bool
copyFrom b =
  let t = B.selectedText b
   in if T.null t then pure False else setClipboard t

orLine :: Buffer -> Buffer
orLine b = if B.hasSelection b then b else B.selectLineAt (B.bufCursor b) b
