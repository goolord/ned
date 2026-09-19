-- | Drives the application in a hidden window on scripted input, checks what
-- the editing did, and writes screenshots of it. Run with
-- @ned --selftest DIR@.
module Ned.Selftest (selftest) where

import Control.Exception (SomeException, try)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr, castPtr)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (newIORef, readIORef)
import qualified Data.Text as T
import qualified Data.Text.NanoRope as Rope
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Runner (shouldRedrawFrame)
import NanoUI.Testing (newPixelContext, uiCursorKind)
import qualified Ned.Buffer as B
import Ned.App
import Ned.View (Editor (..))
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | A Windows build of an SDL program has no console to print to, so the
-- outcome goes to @selftest.log@ in the directory as well.
selftest :: FilePath -> Maybe FilePath -> IO ()
selftest dir mfile = do
  createDirectoryIfMissing True dir
  let logFile = dir </> "selftest.log"
  writeFile logFile ""
  result <- try (selftestIn dir mfile (\line -> appendFile logFile (line <> "\n") >> putStrLn line))
  case result of
    Right () -> pure ()
    Left (e :: SomeException) -> do
      appendFile logFile ("FAILED: " <> show e <> "\n")
      hPutStrLn stderr ("FAILED: " <> show e)
      exitFailure

foreign import ccall unsafe "SDL_SetWindowSize"
  sdlSetWindowSize :: Ptr () -> CInt -> CInt -> IO CBool

selftestIn :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
selftestIn dir mfile say = do
  ctx0 <- newPixelContext >>= (`withTheme` tomorrowNightMinDarkTheme)
  tLoad0 <- getMonotonicTime
  app0 <- maybe (pure newApp) (`openPath` newApp) mfile
  tLoad1 <- B.lineCount (edBuffer (appEditor app0)) `seq` getMonotonicTime
  say (printf "loaded %d lines in %.1f ms" (B.lineCount (edBuffer (appEditor app0))) ((tLoad1 - tLoad0) * 1000))
  ref <- newIORef app0
  let size = Size 1100 760
  withSdl defaultSdlOptions {sdlWindowHidden = True, sdlAppVsync = False, sdlWindowSize = size, sdlWindowResizable = False} ctx0 $ \ctx env -> do
    let base = emptyInput {inputWindowSize = size, inputMousePos = V2 600 400}
        frame inp = void (sdlDrawFrame ctx (appView ref) env inp False)
        idle = frame base >> frame base
        typed t = frame base {inputChars = t} >> idle
        chord c = frame base {inputChars = T.singleton c, inputModifiers = Modifiers False True False} >> idle
        key mods k = frame base {inputKeys = inputKeysFromList [k], inputModifiers = mods} >> idle
        plain = Modifiers False False False
        shot name = do
          frame base
          ok <- saveScreenshot env (dir </> name)
          unless ok (fail ("selftest: could not write " <> name))
        text = Rope.toText . B.bufRope . edBuffer . appEditor <$> readIORef ref
        expect what want = do
          got <- text
          when (got /= want) $
            fail ("selftest: " <> what <> ": expected " <> show want <> ", got " <> show got)

    idle
    shot "01-open.bmp"

    case mfile of
      Just _ -> do
        -- A press on the number of the last line in view takes that line, and
        -- leaves the view where it is: the caret it puts on the line after
        -- is nothing to scroll to.
        frame base {inputMousePos = V2 20 725, inputMouseDown = True, inputMousePressed = True}
        frame base {inputMousePos = V2 20 725, inputMouseReleased = True}
        idle
        scrolled <- edScrollY . appEditor <$> readIORef ref
        when (scrolled /= 0) $ fail ("selftest: a press on the last line number in view scrolled to " <> show scrolled)
        -- Scroll a loaded file about and time the frames.
        t0 <- getMonotonicTime
        forM_ [1 :: Int .. 200] $ \_ -> frame base {inputScroll = V2 0 1}
        t1 <- getMonotonicTime
        say (printf "200 scrolled frames in %.1f ms (%.2f ms a frame)" ((t1 - t0) * 1000) ((t1 - t0) * 5))
        shot "02-scrolled.bmp"
        key (Modifiers False True False) KeyEnd
        shot "03-end.bmp"
        t2 <- getMonotonicTime
        forM_ [1 :: Int .. 200] $ \i -> frame base {inputChars = T.singleton (toEnum (97 + i `rem` 26))}
        t3 <- getMonotonicTime
        say (printf "200 typed frames in %.1f ms (%.2f ms a frame)" ((t3 - t2) * 1000) ((t3 - t2) * 5))
        shot "04-typed.bmp"
      Nothing -> do
        typed "main :: IO ()"
        key plain KeyEnter
        typed "main = do"
        key plain KeyEnter
        key plain KeyTab
        typed "putStrLn \"hello\" -- greet"
        expect "typing" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet"
        key plain KeyEnter
        typed "pure ()"
        expect "auto indent" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    pure ()"
        shot "02-typed.bmp"

        chord 'z'
        expect "undo" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    "
        chord 'y'
        expect "redo" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    pure ()"

        -- Select the last line's text and replace it.
        key (Modifiers True False False) KeyHome
        typed "x"
        expect "replace selection" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    x"
        key plain KeyBackspace
        key plain KeyBackspace
        expect "backspace through indent" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n"

        -- Tab must not walk the focus away from the editor.
        key plain KeyTab
        typed "ok"
        expect "tab keeps focus" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    ok"

        -- With nothing selected, copy takes the line; the last has no newline.
        chord 'c'
        chord 'v'
        expect "line copy and paste" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    ok    ok"
        chord 'z'

        -- Find.
        chord 'f'
        typed "main"
        shot "03-find.bmp"
        key plain KeyEnter
        (ln, col) <- B.cursorPosition . edBuffer . appEditor <$> readIORef ref
        when ((ln, col) /= (1, 4)) $ fail ("selftest: find next landed at " <> show (ln, col))
        key plain KeyEscape

        -- Select all, indent, unindent.
        chord 'a'
        key plain KeyTab
        expect "indent selection" "    main :: IO ()\n    main = do\n        putStrLn \"hello\" -- greet\n        ok"
        key (Modifiers True False False) KeyTab
        expect "unindent selection" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    ok"
        shot "04-selected.bmp"

        -- A click places the caret; a drag selects.
        frame base {inputMousePos = V2 300 300, inputMouseDown = True, inputMousePressed = True}
        frame base {inputMousePos = V2 300 300, inputMouseReleased = True}
        idle
        dirty <- B.isDirty . edBuffer . appEditor <$> readIORef ref
        unless dirty (fail "selftest: buffer should be dirty")
        shot "05-clicked.bmp"

        -- A press on a line number selects the line; dragging down takes more.
        let selection = B.selectedText . edBuffer . appEditor <$> readIORef ref
            at x y = base {inputMousePos = V2 x y}
        frame (at 20 55) {inputMouseDown = True, inputMousePressed = True}
        one <- selection
        when (one /= "main = do" <> T.singleton (toEnum 10)) $ fail ("selftest: gutter press selected " <> show one)
        frame (at 20 75) {inputMouseDown = True}
        frame (at 20 75) {inputMouseReleased = True}
        two <- selection
        when (length (T.lines two) /= 2) $ fail ("selftest: gutter drag selected " <> show two)
        shot "06-gutter.bmp"

        -- A double click takes the word and a triple click the line, and both
        -- keep them through the frames that hold the button, which report
        -- one click as the session's frames do. "putStrLn" is at 85..150.
        let press n x y = frame (at x y) {inputMouseDown = True, inputMousePressed = True, inputMouseClicks = n}
            hold x y = frame (at x y) {inputMouseDown = True}
            release x y = frame (at x y) {inputMouseReleased = True} >> idle
        press 2 100 73 >> hold 100 73 >> hold 100 73 >> release 100 73
        word <- selection
        when (word /= "putStrLn") $ fail ("selftest: double click selected " <> show word)
        press 2 100 73 >> hold 100 73 >> hold 180 73 >> release 180 73
        words2 <- selection
        when (words2 /= "putStrLn " <> T.init (T.pack (show ("hello" :: String)))) $ fail ("selftest: double click and drag selected " <> show words2)
        press 3 100 73 >> hold 100 73 >> hold 100 73 >> release 100 73
        line <- selection
        when (line /= "    putStrLn " <> T.pack (show ("hello" :: String)) <> " -- greet" <> T.singleton (toEnum 10)) $
          fail ("selftest: triple click selected " <> show line)

        -- The pointer crossing from the text onto the scrollbar, and doing
        -- nothing else, has to get a frame out of the session and the arrow
        -- for a cursor; likewise onto the line numbers, and back.
        let crossing name from to want = do
              frame (at (fst from) (snd from)) >> frame (at (fst from) (snd from))
              let prevInp = at (fst from) (snd from)
                  curInp = at (fst to) (snd to)
              due <- shouldRedrawFrame ctx prevInp curInp False False False
              unless due $ fail ("selftest: no frame for the pointer moving " <> name)
              frame curInp
              kind <- uiCursorKind ctx curInp
              when (kind /= want) $ fail ("selftest: cursor " <> show kind <> " after moving " <> name)
        crossing "onto the scrollbar" (600, 400) (1094, 400) UiCursorDefault
        crossing "back onto the text" (1094, 400) (600, 400) UiCursorText
        crossing "onto the line numbers" (600, 400) (20, 400) UiCursorDefault

        -- A right click outside the selection moves the caret and opens the menu.
        frame (at 300 40) {inputMouseRightDown = True, inputMouseRightPressed = True}
        frame (at 300 40) {inputMouseRightReleased = True}
        none <- selection
        unless (T.null none) $ fail "selftest: right click kept the selection"
        frame (at 300 40)
        ok <- saveScreenshot env (dir </> "07-context-menu.bmp")
        unless ok (fail "selftest: could not write 07-context-menu.bmp")
        frame (at 900 500) {inputMouseDown = True, inputMousePressed = True}
        frame (at 900 500) {inputMouseReleased = True}

        -- A click on a menu's button has to ask for the frame that shows the
        -- menu: nothing else will, with the pointer at rest.
        _ <- sdlDrawFrame ctx (appView ref) env (at 17 13) {inputMouseDown = True, inputMousePressed = True} False
        (dirty1, _) <- sdlDrawFrame ctx (appView ref) env (at 17 13) {inputMouseReleased = True} False
        opened <- appOpenMenu <$> readIORef ref
        when (opened /= "File") $ fail "selftest: the File menu did not open"
        unless dirty1 $ fail "selftest: opening a menu asked for no frame"
        frame (at 17 13)
        ok2 <- saveScreenshot env (dir </> "08-menu.bmp")
        unless ok2 (fail "selftest: could not write 08-menu.bmp")

        -- A click on the open menu's button closes it, and the next opens it.
        let menuNow = appOpenMenu <$> readIORef ref
            clickFile = do
              frame (at 17 13) {inputMouseDown = True, inputMousePressed = True}
              frame (at 17 13) {inputMouseReleased = True}
              idle
        clickFile
        closed <- menuNow
        when (closed /= "") $ fail ("selftest: a click on the open menu's button left " <> show closed <> " open")
        clickFile
        reopened <- menuNow
        when (reopened /= "File") $ fail "selftest: a click on a closed menu's button did not open it"
        clickFile
    -- Resize the window a step at a time, as a drag of its border does, and
    -- time the frames; then the same under a view of one label, for what the
    -- toolkit itself spends on a new size.
    let resizeRun name ui = do
          t0 <- getMonotonicTime
          forM_ [1 :: Int .. 100] $ \i -> do
            _ <- sdlSetWindowSize (castPtr (sdlWindow env)) (fromIntegral (1500 + 8 * i)) (fromIntegral (900 + 4 * i))
            (ctx', inp') <- syncDisplay ctx env base
            when (i == 100) (say ("  window is now " <> show (inputWindowSize inp')))
            void (sdlDrawFrame ctx' ui env inp' True)
          t1 <- getMonotonicTime
          say (printf "100 resized frames, %s: %.1f ms (%.2f ms a frame)" (name :: String) ((t1 - t0) * 1000) ((t1 - t0) * 10))
    resizeRun "editor" (appView ref)
    resizeRun "one label" (label "resize")
    say "ned selftest: ok"
