-- | Tests of the editor's core, which needs no window: the buffer against a
-- model kept as one 'Text', the history, search, the lexer, and files.
module Main (main) where

import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.NanoRope as Rope
import Ned.File (Eol (..), FileFormat (..), Loaded (..), loadFile, saveFile)
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Highlight
import System.Directory (getTemporaryDirectory, removeFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import qualified Data.ByteString as BS

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check :: (Eq a, Show a) => String -> a -> a -> IO ()
      check = expect failures

  -- Editing ------------------------------------------------------------------
  let typed = foldl' (\b c -> B.insertText (T.singleton c) b) B.empty ("foo bar" :: String)
  check "typing" "foo bar" (text typed)
  check "undo takes a word back" "foo" (text (B.undo typed))
  check "redo" "foo bar" (text (B.redo (B.redo (B.undo (B.undo typed)))))
  check "dirty after typing" True (B.isDirty typed)
  check "clean after undoing everything" False (B.isDirty (B.undo (B.undo typed)))
  check "clean once saved" False (B.isDirty (B.markSaved typed))

  let doc = B.fromText "alpha\n  beta gamma\n\tdelta\n"
  check "line count" 4 (B.lineCount doc)
  check "line text" "  beta gamma" (B.lineText doc 1)
  check "line length" 6 (B.lineLength doc 2)
  check "last line is empty" 0 (B.lineLength doc 3)
  check "tabs detected" True (B.usesTabs doc)
  check "tab is four cells" 4 (B.colToVisual doc 2 1)
  check "cell back to column" 1 (B.visualToCol doc 2 4)
  check "cell inside a tab rounds" 0 (B.visualToCol doc 2 1)
  check "wide characters take two cells" 4 (B.colToVisual (B.fromText "\x4E2D\x6587") 0 2)

  let atBeta = B.setCursor False 8 doc
  check "position" (1, 2) (B.cursorPosition atBeta)
  check "down keeps the cell, which is the far half of a tab" (2, 1) (B.cursorPosition (B.moveDown False atBeta))
  check "down and up return" (1, 2) (B.cursorPosition (B.moveUp False (B.moveDown False atBeta)))
  check "home goes to the indent" (1, 2) (B.cursorPosition (B.moveHome False (B.moveEnd False atBeta)))
  check "home again goes to the margin" (1, 0) (B.cursorPosition (B.moveHome False (B.moveHome False (B.moveEnd False atBeta))))
  check "word right" (1, 7) (B.cursorPosition (B.moveWordRight False atBeta))
  check "word left" (1, 2) (B.cursorPosition (B.moveWordLeft False (B.moveWordRight False atBeta)))
  check "select word" "beta" (B.selectedText (B.selectWordAt 9 doc))
  check "select line" "  beta gamma\n" (B.selectedText (B.selectLineAt 9 doc))
  check "typing replaces the selection" "alpha\n  X gamma\n\tdelta\n" (text (B.insertText "X" (B.selectWordAt 9 doc)))
  check "newline carries the indent" "alpha\n  beta\n   gamma\n\tdelta\n" (text (B.newline (B.setCursor False 12 doc)))
  check "pasted line endings become newlines" "a\nb\nc" (text (B.insertText "a\r\nb\nc" B.empty))
  check "typing nothing leaves the selection be" ("beta", False) (let b = B.insertText "" (B.selectWordAt 9 doc) in (B.selectedText b, B.isDirty b))
  check "go to line" (2, 0) (B.cursorPosition (B.gotoLine 3 doc))
  check "go to line clamps" (3, 0) (B.cursorPosition (B.gotoLine 99 doc))

  let spaces = B.fromText "        x"
  check "backspace in indentation goes to the tab stop" "    x" (text (B.backspace (B.setCursor False 8 spaces)))
  check "tab goes to the tab stop" "ab  " (text (B.indentKey (B.insertText "ab" B.empty)))
  let block = B.selectAll (B.fromText "one\ntwo\nthree")
  check "indent lines" "    one\n    two\n    three" (text (B.indentKey block))
  check "unindent lines" "one\ntwo\nthree" (text (B.unindentKey (B.indentKey block)))
  check "indenting lines is one step" "one\ntwo\nthree" (text (B.undo (B.indentKey block)))
  let undoSteps b = length (takeWhile B.canUndo (iterate B.undo b))
  check "indenting lines keeps to the undo limit" True (undoSteps (iterate B.indentKey block !! 4100) <= 4000)
  let caretIn = B.setCursor False 6 (B.fromText "    indented")
  check "shift+tab with a caret unindents" "indented" (text (B.unindentKey caretIn))
  check "and leaves a caret, moved with the text" (2, 2) (B.bufCursor (B.unindentKey caretIn), B.bufAnchor (B.unindentKey caretIn))
  check "delete word back" "foo " (text (B.deleteWordBack typed))

  -- Search -------------------------------------------------------------------
  let hay = B.fromText (T.replicate 30000 "abc " <> "Needle" <> T.replicate 30000 " xyz" <> "needle")
  check "find ignores case" (Just 120000) (B.bufAnchor <$> B.findNext False "needle" hay)
  check "find exact" (Just 240006) (B.bufAnchor <$> B.findNext True "needle" hay)
  check "find wraps" (Just 120000) (B.bufAnchor <$> (B.findNext False "needle" hay >>= B.findNext False "needle" >>= B.findNext False "needle"))
  check "find backwards wraps" (Just 240006) (B.bufAnchor <$> B.findPrev False "needle" hay)
  check "find nothing" Nothing (B.bufAnchor <$> B.findNext False "absent" hay)
  -- A match lying across the seam of two search windows.
  let seam = B.fromText (T.replicate 65530 "." <> "straddle" <> "....")
  check "find across windows" (Just 65530) (B.bufAnchor <$> B.findNext True "straddle" seam)
  check "find back across windows" (Just 65530) (B.bufAnchor <$> B.findPrev True "straddle" (B.moveDocEnd False seam))

  -- A needle longer than a search window.
  let big = T.replicate 70000 "z"
  check "find a needle longer than the window" (Just 3) (B.bufAnchor <$> B.findNext True big (B.fromText ("abc" <> big <> "def")))
  check "and miss one" Nothing (B.bufAnchor <$> B.findNext True (big <> "q") (B.fromText (T.replicate 200000 "z")))
  check "and backwards" (Just 3) (B.bufAnchor <$> B.findPrev True big (B.moveDocEnd False (B.fromText ("abc" <> big <> "def"))))

  -- Long lines ---------------------------------------------------------------
  let long = B.fromText (T.replicate 100000 "x" <> "\nshort")
  check "long line" True (B.isLongLine long 0)
  check "long line window" "xxxx" (B.lineWindow long 0 50000 50004)
  check "long line cells are columns" 70000 (B.colToVisual long 0 70000)
  check "short line after it" "short" (B.lineText long 1)

  -- The buffer against a model -----------------------------------------------
  modelRun failures 20000

  -- Lexing -------------------------------------------------------------------
  let hs = languageFor "Main.hs"
      kinds l st t = [(T.take n (T.drop o t), k) | (o, Span n k) <- offsets (fst (lexLine l st t)), k /= TokPlain]
      offsets spans = zip (scanl (+) 0 (map spanLength spans)) spans
  check "language by name" "Makefile" (langName (languageFor "src/Makefile"))
  check "language by a name that has an extension" "Cabal" (langName (languageFor "cabal.project"))
  check "extensions in any case" "C++" (langName (languageFor "src/A.HPP"))
  check "unknown is plain" "Plain Text" (langName (languageFor "notes.xyz"))
  check
    "haskell line"
    [("import", TokKeyword), ("Data.Text", TokModule), ("(", TokPunct), ("Text", TokType), (")", TokPunct), ("-- note", TokComment)]
    (kinds hs LexNormal "import Data.Text (Text) -- note")
  check "string with an escape" [("\"a\\\"b\"", TokString), ("<>", TokPunct)] (kinds hs LexNormal "\"a\\\"b\" <> x")
  check "character, and a prime that is not one" [("'x'", TokString)] (kinds hs LexNormal "  foo' 'x'")
  check "a quote is a character of its own" [("==", TokPunct), ("'\\''", TokString)] (kinds hs LexNormal "  x == '\\''")
  check
    "the head of an application, qualified"
    [("<-", TokPunct), ("T.", TokModule), ("length", TokFunction), ("(", TokPunct), ("f", TokFunction), (")", TokPunct)]
    (kinds hs LexNormal "  n <- T.length (f x y)")
  check "a qualified argument" [("map", TokFunction), ("T.", TokModule)] (kinds hs LexNormal "  map T.length xs")
  check "a definition" [("go", TokFunction), ("=", TokPunct), ("case", TokKeyword), ("of", TokKeyword)] (kinds hs LexNormal "go acc t = case t of")
  check "a signature applies types" [("f", TokFunction), ("::", TokPunct), ("Maybe", TokType), ("->", TokPunct)] (kinds hs LexNormal "f :: Maybe a -> m b")
  check "a lambda's parameters" [("\\", TokPunct), ("->", TokPunct), ("g", TokFunction)] (kinds hs LexNormal "  \\x y -> g x")
  check "block comment opens" (LexBlock 1) (lexState hs LexNormal "x {- start")
  check "block comment nests" (LexBlock 2) (lexState hs (LexBlock 1) "still {- deeper")
  check "block comment closes" LexNormal (lexState hs (LexBlock 1) "done -} x")
  check "inside a block comment" [("all of this", TokComment)] (kinds hs (LexBlock 1) "all of this")
  check "an empty line leaves the state alone" (LexBlock 1) (lexState hs (LexBlock 1) "")
  check "string gap opens" (LexGap '"') (lexState hs LexNormal "  let kw = \"add all \\")
  check "an escaped backslash opens no gap" LexNormal (lexState hs LexNormal "  let kw = \"add all \\\\")
  check "string gap carries on" [("\\case else \\", TokString)] (kinds hs (LexGap '"') "    \\case else \\")
  check "string gap carries on, state" (LexGap '"') (lexState hs (LexGap '"') "    \\case else \\")
  check "string gap over a blank line" (LexGap '"') (lexState hs (LexGap '"') "   ")
  check
    "string resumes and ends"
    [("\\then\"", TokString), ("<>", TokPunct)]
    (kinds hs (LexGap '"') "    \\then\" <> x")
  check "string ends where it resumes" [("\\\"", TokString), ("<>", TokPunct)] (kinds hs (LexGap '"') "  \\\" <> x")
  check "string gap opens before trailing space" (LexGap '"') (lexState hs LexNormal "  let kw = \"add all \\  ")
  check "no gap after all" (kinds hs LexNormal "main = do") (kinds hs (LexGap '"') "main = do")
  check "no string gaps in c" LexNormal (lexState (languageFor "a.c") LexNormal "char *s = \"abc\\")
  let py = languageFor "a.py"
  check "python string over lines" (LexString "\"\"\"") (lexState py LexNormal "x = \"\"\"doc")
  check "python string ends" LexNormal (lexState py (LexString "\"\"\"") "end\"\"\" + 1")
  check "call and number" [("print", TokFunction), ("(", TokPunct), ("0x1F", TokNumber), (")", TokPunct)] (kinds py LexNormal "print(0x1F)")
  let lua = languageFor "a.lua"
  check "lua block comment opens" (LexBlock 1) (lexState lua LexNormal "x = 1 --[[ start")
  check "lua block comment closes" LexNormal (lexState lua (LexBlock 1) "end ]] y = 2")
  check "lua line comment" LexNormal (lexState lua LexNormal "x = 1 -- note")
  check "lua long string opens" (LexString "]]") (lexState lua LexNormal "x = [[ long")
  check "lua long string closes" LexNormal (lexState lua (LexString "]]") "more ]] y = 2")
  check "lua long string on one line" [("=", TokPunct), ("[[ long ]]", TokString)] (kinds lua LexNormal "x = [[ long ]]")
  check "spans cover the line" 31 (sum (map spanLength (fst (lexLine hs LexNormal "import Data.Text (Text) -- note"))))
  check "c directive" [("#include", TokKeyword), ("<", TokPunct), (".", TokPunct), (">", TokPunct)] (kinds (languageFor "a.c") LexNormal "#include <stdio.h>")

  -- Files --------------------------------------------------------------------
  tmp <- getTemporaryDirectory
  let path = tmp </> "ned-test-roundtrip.txt"
  BS.writeFile path "\xEF\xBB\xBFone\r\ntwo\r\nthr\xC3\xA9\&e\r\n"
  loaded <- loadFile path
  case loaded of
    Left err -> check "load" "" err
    Right (Loaded buf format lossy) -> do
      check "format detected" (FileFormat CRLF True) format
      check "valid UTF-8 is not lossy" False lossy
      check "loaded as newlines, without the mark" "one\ntwo\nthr\233e\n" (text buf)
      saved <- saveFile path format (B.insertText "zero\n" buf)
      check "saved" (Right ()) saved
      bytes <- BS.readFile path
      check "line endings and the mark restored" "\xEF\xBB\xBF\&zero\r\none\r\ntwo\r\nthr\xC3\xA9\&e\r\n" bytes
      _ <- saveFile path (FileFormat LF False) buf
      bytesLf <- BS.readFile path
      check "saved with newlines" "one\ntwo\nthr\xC3\xA9\&e\n" bytesLf
  -- A file that is not UTF-8 opens, and says that it lost something.
  BS.writeFile path "caf\xE9\n"
  latin <- loadFile path
  check "not UTF-8 is lossy" (Right True) (loadedLossy <$> latin)
  check "not UTF-8 shows replacements" (Right "caf\xFFFD\n") (text . loadedBuffer <$> latin)
  removeFile path

  n <- readIORef failures
  if n == 0
    then putStrLn "ned-test: all passed"
    else putStrLn ("ned-test: " <> show n <> " failed") >> exitFailure

text :: Buffer -> Text
text = Rope.toText . B.bufRope

expect :: (Eq a, Show a) => IORef Int -> String -> a -> a -> IO ()
expect failures name want got =
  unless (want == got) $ do
    modifyIORef' failures (+ 1)
    putStrLn ("FAIL " <> name <> "\n  expected " <> show want <> "\n  got      " <> show got)

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

-- | The editor as one text and two offsets.
data Model = Model !Text !Int !Int

-- | Run random edits and movements on a buffer and on the model, and compare
-- text, caret and anchor after each.
modelRun :: IORef Int -> Int -> IO ()
modelRun failures steps = go steps (12345 :: Int) B.empty (Model T.empty 0 0)
  where
    go 0 _ _ _ = pure ()
    go n seed b m = do
      let seed' = (seed * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (62 :: Int))
          r = seed' `div` 65536
          arg = (r `div` 16) `mod` 97
          (b', m', name) = step (r `mod` 9) arg b m
          Model t c a = m'
          ok = text b' == t && B.bufCursor b' == c && B.bufAnchor b' == a
      if ok
        then go (n - 1 :: Int) seed' b' m'
        else do
          modifyIORef' failures (+ 1)
          putStrLn ("FAIL model after " <> name <> " at step " <> show (steps - n))
          putStrLn ("  buffer " <> show (T.take 80 (text b'), B.bufCursor b', B.bufAnchor b'))
          putStrLn ("  model  " <> show (T.take 80 t, c, a))

    step :: Int -> Int -> Buffer -> Model -> (Buffer, Model, String)
    step op arg b (Model t c a) =
      let lo = min c a
          hi = max c a
          len = T.length t
          splice i j new = Model (T.take i t <> new <> T.drop j t) (i + T.length new) (i + T.length new)
          sample = T.take (1 + arg `mod` 5) (T.drop (arg `mod` 7) "ab\ncd ef\n\tgh")
       in case op of
            0 -> (B.insertText sample b, splice lo hi sample, "insert")
            1 -> (B.insertText "q" b, splice lo hi "q", "type")
            2 ->
              let off = arg * len `div` 97
               in (B.setCursor False off b, Model t off off, "set cursor")
            3 ->
              let off = arg * len `div` 97
               in (B.setCursor True off b, Model t off a, "extend")
            4
              | lo /= hi -> (B.deleteForward b, splice lo hi T.empty, "delete selection")
              | otherwise -> (B.deleteForward b, splice c (min len (c + 1)) T.empty, "delete")
            5
              | lo /= hi -> (B.moveLeft False b, Model t lo lo, "left collapses")
              | otherwise -> (B.moveLeft False b, Model t (max 0 (c - 1)) (max 0 (c - 1)), "left")
            6 -> (B.moveRight True b, Model t (min len (c + 1)) a, "right extending")
            7 -> (B.moveDocEnd False b, Model t len len, "end")
            _ -> (B.selectAll b, Model t len 0, "select all")
