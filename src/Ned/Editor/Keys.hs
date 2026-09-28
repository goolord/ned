-- | The keyboard, and the clipboard it reaches for.
--
-- Every key the editor answers to on its own is here: the movements, the
-- edits, and the chords that are the text's rather than the application's.
-- The chords the application owns -- save, open, find -- never reach this
-- module, so there is one place to read to know what a key does to the text.
module Ned.Editor.Keys
  ( applyKeys
  , clipboardCopy
  , clipboardCut
  , clipboardPaste
  ) where

import Control.Monad (foldM)
import qualified Data.Text as T
import NanoUI
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B

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
