-- | Drives the application in a hidden window on scripted input, checks what
-- the editing did, and writes screenshots of it. Run with
-- @ned --selftest DIR@.
--
-- The finder's own thread is the one thing here that runs on the clock rather
-- than on frames, so the part that tests it waits on it between frames.
module Ned.Selftest (selftest) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.NanoRope as Rope
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Context (Context (..), getWakeAt)
import NanoUI.Testing (cursorKindIs, newPixelContext, uiCursorKind)
import qualified Ned.Buffer as B
import Ned.App
import qualified Ned.FileTree as FT
import qualified Ned.Picker as P
import Ned.Editor (Editor (..), cellWidth, defaultFontSize)
import Ned.View (appView)
import System.Directory (createDirectoryIfMissing, findExecutable, makeAbsolute)
import System.Exit (exitFailure)
import System.FilePath (equalFilePath, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import NanoUI.Backend (lineWidthIO)
import NanoUI.Input (emptyInput)
import NanoUI.Input (inputKeysFromList)

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

selftestIn :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
selftestIn dir mfile say = do
  ctx0 <- newPixelContext >>= (`withTheme` tomorrowNightMinDarkTheme)
  blankApp <- newAppIn
  tLoad0 <- getMonotonicTime
  app0 <- maybe (pure blankApp) (`openPath` blankApp) mfile
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
        at x y = base {inputMousePos = V2 x y}
        -- A click is a press and, a frame later, the release.
        tap x y = do
          frame (at x y) {inputMouseDown = True, inputMousePressed = True}
          frame (at x y) {inputMouseReleased = True}
        click x y = tap x y >> idle
        -- A screenshot of the window as it stands, or with the pointer put
        -- back where it rests.
        snap name = do
          ok <- saveScreenshot env (dir </> name)
          unless ok (fail ("selftest: could not write " <> name))
        shot name = frame base >> snap name
        text = Rope.toText . B.bufRope . edBuffer . appEditor <$> readIORef ref
        expect what want = do
          got <- text
          when (got /= want) $
            fail ("selftest: " <> what <> ": expected " <> show want <> ", got " <> show got)

    idle
    shot "01-open.bmp"

    -- Everything below places the pointer by what the editor's own rectangle
    -- holds, so the tree is put away first and taken up again at the end.
    treeShown <- appTreeShown <$> readIORef ref
    unless treeShown (fail "selftest: the file tree should start out shown")
    chord 'b'
    stillShown <- appTreeShown <$> readIORef ref
    when stillShown (fail "selftest: Ctrl+B did not put the file tree away")

    -- A run drawn as one op has to end where its cells do, or the span after
    -- it is drawn over its tail. At the sizes zooming passes through, where
    -- the advance a glyph reports and the one a run is laid out by differ by
    -- other fractions.
    forM_ [8, 11, defaultFontSize, 16.5, 20, 33, 48] $ \pt -> do
      (fm, _) <- ctxResolveFont ctx pt WeightNormal FontStyleNormal FontMono
      cellW <- cellWidth fm
      let run = T.replicate 150 "e"
      drawn <- lineWidthIO fm run
      when (abs (drawn - 150 * cellW) > 1) $
        fail (printf "selftest: at size %.1f a run of 150 cells is %.2f wide and is drawn %.2f wide" pt (150 * cellW) drawn)

    case mfile of
      Just _ -> do
        -- A press on the number of the last line in view takes that line, and
        -- leaves the view where it is: the caret it puts on the line after
        -- is nothing to scroll to.
        click 20 725
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

        -- A press on the text takes the keyboard from the go-to-line field,
        -- and a press on the field takes it back, as the find field does.
        let barHasKeys = appBarFocus <$> readIORef ref
        chord 'g'
        click 600 300
        lostKeys <- not <$> barHasKeys
        unless lostKeys (fail "selftest: a press on the text left the keyboard in the go-to-line field")
        click 400 707
        tookKeys <- barHasKeys
        unless tookKeys (fail "selftest: a press on the go-to-line field did not give it the keyboard")
        typed "2"
        key plain KeyEnter
        (gotoLn, _) <- B.cursorPosition . edBuffer . appEditor <$> readIORef ref
        when (gotoLn /= 1) $ fail ("selftest: go to line 2 landed on line " <> show (gotoLn + 1))

        -- Select all, indent, unindent.
        chord 'a'
        key plain KeyTab
        expect "indent selection" "    main :: IO ()\n    main = do\n        putStrLn \"hello\" -- greet\n        ok"
        key (Modifiers True False False) KeyTab
        expect "unindent selection" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    ok"
        shot "04-selected.bmp"

        -- A click places the caret; a drag selects.
        click 300 300
        dirty <- B.isDirty . edBuffer . appEditor <$> readIORef ref
        unless dirty (fail "selftest: buffer should be dirty")
        shot "05-clicked.bmp"

        -- A press on a line number selects the line; dragging down takes more.
        let selection = B.selectedText . edBuffer . appEditor <$> readIORef ref
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

        -- What is under the pointer says what the cursor is: the text has the
        -- beam, the scrollbar and the line numbers the arrow.
        let crossing name x want = do
              frame (at x 400) >> frame (at x 400)
              kind <- uiCursorKind ctx (at x 400)
              when (kind /= want) $ fail ("selftest: cursor " <> show kind <> " " <> name)
        crossing "on the scrollbar" 1094 UiCursorDefault
        crossing "on the text" 600 UiCursorText
        crossing "on the line numbers" 20 UiCursorDefault

        -- A right click outside the selection moves the caret and opens the menu.
        frame (at 300 40) {inputMouseRightDown = True, inputMouseRightPressed = True}
        frame (at 300 40) {inputMouseRightReleased = True}
        none <- selection
        unless (T.null none) $ fail "selftest: right click kept the selection"
        frame (at 300 40)
        snap "07-context-menu.bmp"
        tap 900 500

        -- A click on a menu's button has to ask for the frame that shows the
        -- menu: nothing else will, with the pointer at rest.
        _ <- sdlDrawFrame ctx (appView ref) env (at 17 13) {inputMouseDown = True, inputMousePressed = True} False
        (dirty1, _) <- sdlDrawFrame ctx (appView ref) env (at 17 13) {inputMouseReleased = True} False
        opened <- appOpenMenu <$> readIORef ref
        when (opened /= "File") $ fail "selftest: the File menu did not open"
        unless dirty1 $ fail "selftest: opening a menu asked for no frame"
        frame (at 17 13)
        snap "08-menu.bmp"

        -- A click on the open menu's button closes it, and the next opens it.
        let menuNow = appOpenMenu <$> readIORef ref
            clickFile = click 17 13
        clickFile
        closed <- menuNow
        when (closed /= "") $ fail ("selftest: a click on the open menu's button left " <> show closed <> " open")
        clickFile
        reopened <- menuNow
        when (reopened /= "File") $ fail "selftest: a click on a closed menu's button did not open it"
        clickFile

    -- The file tree, on a folder made here so that what it lists is known.
    -- The tree is put on it by hand, as opening a file in it would be.
    treeDir <- makeAbsolute (dir </> "tree")
    createDirectoryIfMissing True (treeDir </> "sub")
    writeFile (treeDir </> "sub" </> "inner.txt") "inner\n"
    writeFile (treeDir </> "outer.txt") "outer\n"
    -- A file the tree opens goes through the guard every other open does, and
    -- the text here has changes; they are put down rather than asked about.
    modifyIORef' ref $ \a ->
      a {appEditor = (appEditor a) {edBuffer = B.markSaved (edBuffer (appEditor a))}}
    chord 'b'
    modifyIORef' ref (\a -> a {appTree = FT.setRoot treeDir (appTree a)})
    idle
    let treeNow = appTree <$> readIORef ref
        names = map FT.rowName . toList . FT.ftRows <$> treeNow
        expectRows what want = do
          got <- names
          when (got /= want) $
            fail ("selftest: " <> what <> ": expected " <> show want <> ", got " <> show got)
    expectRows "the tree's rows" ["sub", "outer.txt"]

    -- A press on the tree takes the keyboard from the find bar's field, as a
    -- press on the text does.
    chord 'f'
    click 100 600
    findKeptKeys <- appBarFocus <$> readIORef ref
    when findKeptKeys (fail "selftest: a press on the tree left the keyboard in the find field")
    key plain KeyEscape

    -- A press inside the tree, below its rows, gives it the keyboard without
    -- taking anything; from there the arrows walk it.
    click 100 600
    key plain KeyDown
    key plain KeyRight
    expectRows "a folder opened with Right" ["sub", "inner.txt", "outer.txt"]
    shot "09-tree.bmp"
    key plain KeyLeft
    expectRows "a folder closed with Left" ["sub", "outer.txt"]

    -- Enter on a file opens it, and the tree keeps the folder it is on.
    key plain KeyRight
    key plain KeyDown
    key plain KeyEnter
    idle
    openedPath <- appPath <$> readIORef ref
    unless (maybe False (equalFilePath (treeDir </> "sub" </> "inner.txt")) openedPath) $
      fail ("selftest: Enter in the tree opened " <> show openedPath)
    opened <- text
    when (opened /= "inner\n") $ fail ("selftest: the tree opened a file holding " <> show opened)
    root <- FT.ftRoot <$> treeNow
    when (root /= treeDir) $ fail ("selftest: opening a file in the tree moved it to " <> show root)
    expectRows "the tree after opening a file" ["sub", "inner.txt", "outer.txt"]
    treeHasKeys <- appTreeFocus <$> readIORef ref
    when treeHasKeys (fail "selftest: opening a file left the keyboard in the tree")
    typed "x"
    typedInto <- text
    when (typedInto == opened) $ fail "selftest: the text took nothing after the tree opened it"

    -- Every row of the tree is the one widget, so nano-ui runs no frame for a
    -- pointer that crosses from one row to the next, and the tree has to ask
    -- for one itself. Without that the row drawn under the pointer waits for
    -- whatever wants the next frame, which between caret blinks is half a
    -- second.
    frame base {inputMousePos = V2 60 70}
    wakeNow <- getMonotonicTime
    wakeAt <- getWakeAt ctx
    when (wakeAt <= 0 || wakeAt - wakeNow > 0.1) $
      fail (printf "selftest: the tree asked for no frame with the pointer over it (in %.3f s)" (wakeAt - wakeNow))
    -- The caret asks for a frame of its own at the next blink, which is up to
    -- a blink away and would be taken for the tree's. Moving it first puts
    -- that a whole blink off, so a frame wanted sooner than this is the
    -- tree's and nothing else.
    key plain KeyHome
    frame base {inputMousePos = V2 900 400}
    awayNow <- getMonotonicTime
    awayWake <- getWakeAt ctx
    when (awayWake > 0 && awayWake - awayNow < 0.1) $
      fail (printf "selftest: the tree asked for a frame in %.3f s with the pointer off it" (awayWake - awayNow))

    -- A press on a folder's row closes it again. The rows start under the
    -- menu bar and the tree's own heading, a row every line height.
    click 60 70
    expectRows "a folder closed by a press on it" ["sub", "outer.txt"]

    -- Putting the tree away maximizes the editor's pane and taking it up again
    -- restores the split, so the tree comes back at the width it was left at.
    -- The bar between them is the pane grid's divider, found by its cursor.
    let atX x = base {inputMousePos = V2 x 300}
        findDivider x
          | x > 900 = fail "selftest: found no bar between the tree and the text"
          | otherwise = do
              frame (atX x)
              here <- cursorKindIs ctx (atX x) UiCursorEwResize
              if here then pure x else findDivider (x + 1)
    was <- findDivider 40
    chord 'b'
    chord 'b'
    idle
    back <- findDivider 40
    when (back /= was) $
      fail (printf "selftest: the tree came back at %.0f after being put away, not %.0f" back was)

    -- A folder holding more than the view does scrolls, and the wheel moves
    -- it the way it moves the text.
    forM_ [1 :: Int .. 60] $ \i -> writeFile (treeDir </> printf "file-%02d.txt" i) ""
    modifyIORef' ref (\a -> a {appTree = FT.refresh (appTree a)})
    idle
    frame base {inputMousePos = V2 100 300, inputScroll = V2 0 4}
    idle
    scrolledTree <- FT.ftScroll <$> treeNow
    when (scrolledTree <= 0) $ fail ("selftest: the wheel left the tree at " <> show scrolledTree)
    shot "10-tree-scrolled.bmp"

    -- The fuzzy finder, over the same folder the tree is on. It walks the
    -- folder on a thread of its own, so the frames go round until it says it
    -- has found everything; nothing else in the window waits on it.
    chord 'p'
    let pickerNow = appPicker <$> readIORef ref
        settle :: Int -> IO P.Picker
        settle 0 = fail "selftest: the finder never finished looking"
        settle k =
          idle >> pickerNow >>= \case
            Just pk | P.pickerDone pk -> pure pk
            Just _ -> threadDelay 20000 >> settle (k - 1)
            Nothing -> fail "selftest: Ctrl+P did not put the finder up"
        landedOn what name pk =
          unless (maybe False ((== name) . P.itemText) (P.pickerCurrent pk)) $
            fail ("selftest: " <> what <> " landed on " <> show (P.itemText <$> P.pickerCurrent pk))
        clearQuery n = forM_ [1 .. n :: Int] (\_ -> key plain KeyBackspace) >> idle
    gathered <- settle 200
    -- The folder holds 60 numbered files, outer.txt, and inner.txt inside sub.
    when (P.pickerGathered gathered /= 62) $
      fail ("selftest: the finder found " <> show (P.pickerGathered gathered) <> " files, not 62")
    shot "11-picker.bmp"

    -- A query narrows the rows, and the row the keyboard is on is previewed.
    typed "inner"
    idle
    narrowed <- settle 20
    when (P.pickerCount narrowed /= 1) $
      fail ("selftest: \"inner\" matched " <> show (P.pickerCount narrowed) <> " files, not 1")
    landedOn "the query \"inner\"" "sub/inner.txt" narrowed
    shot "12-picker-query.bmp"

    -- Enter opens what the keyboard is on and puts the finder away. The text
    -- has changes typed into it above, which are put down rather than asked
    -- about, as they are everywhere else in this test.
    modifyIORef' ref $ \a ->
      a {appEditor = (appEditor a) {edBuffer = B.markSaved (edBuffer (appEditor a))}}
    key plain KeyEnter
    idle
    pickedPath <- appPath <$> readIORef ref
    unless (maybe False (equalFilePath (treeDir </> "sub" </> "inner.txt")) pickedPath) $
      fail ("selftest: the finder opened " <> show pickedPath)
    stillUp <- pickerNow
    when (isJust stillUp) (fail "selftest: picking a file left the finder up")

    -- A file with something in it, to see the preview colour it. A finder
    -- gathers when it opens, so the file is written before this one does.
    writeFile (treeDir </> "demo.hs") $
      unlines $
        [ "-- | A module the preview has something to colour."
        , "module Demo (greet) where"
        , ""
        , "greet :: String -> IO ()"
        , "greet name = putStrLn (\"hello, \" <> name <> \"!\")"
        , ""
        , "-- A line long enough that the preview has to cut it off where the pane ends rather than draw it on over the rows beside it."
        , "numbers :: [Int]"
        , "numbers = [1 .. 40]"
        ]
          -- More lines than the pane holds, so that it has somewhere to scroll.
          <> concat [["", "line" <> show i <> " :: Int", "line" <> show i <> " = " <> show i] | i <- [1 :: Int .. 20]]
    chord 'p'
    _ <- settle 200
    typed "demo"
    idle
    previewed <- settle 20
    landedOn "the query \"demo\"" "demo.hs" previewed
    shot "13-picker-preview.bmp"

    -- The preview is read rather than walked, so the chords a pager scrolls
    -- with move it and the arrows are left to the rows.
    chord 'd'
    idle
    shot "14-picker-scrolled.bmp"
    chord 'u'
    idle

    -- The arrows and the chords a terminal's finder is walked with both move
    -- the keyboard down the rows.
    -- An empty query keeps the rows in the order they were gathered in, which
    -- is the order a directory is listed in: demo.hs, then the numbered files.
    clearQuery 4
    cleared <- settle 20
    when (P.pickerCount cleared /= 63) $
      fail ("selftest: an empty query kept " <> show (P.pickerCount cleared) <> " rows, not 63")
    landedOn "an emptied query" "demo.hs" cleared
    key plain KeyDown
    key plain KeyDown
    chord 'n'
    walked <- settle 20
    landedOn "three steps down the rows" "file-03.txt" walked
    chord 'p'
    stepped <- settle 20
    landedOn "a step back up the rows" "file-02.txt" stepped

    -- The rows' scrollbar is down the right of them. Its thumb, held and
    -- dragged down, scrolls them; the button coming up lets go of it.
    frame (at 462 130) {inputMouseDown = True, inputMousePressed = True}
    frame (at 462 400) {inputMouseDown = True}
    frame (at 462 400) {inputMouseReleased = True}
    idle
    dragged <- settle 20
    when (P.pickerTop dragged <= 0) $
      fail ("selftest: dragging the thumb left the finder's rows at " <> show (P.pickerTop dragged))
    frame (at 462 200)
    idle
    released <- settle 20
    when (P.pickerTop released /= P.pickerTop dragged) $
      fail "selftest: the thumb kept following the pointer after the button came up"

    -- The wheel over the rows scrolls them, and back up again.
    frame base {inputMousePos = V2 300 250}
    frame base {inputMousePos = V2 300 250, inputScroll = V2 0 4}
    idle
    wheeled <- settle 20
    when (P.pickerTop wheeled <= 0) $
      fail ("selftest: the wheel left the finder's rows at " <> show (P.pickerTop wheeled))
    frame base {inputMousePos = V2 300 250, inputScroll = V2 0 (-40)}
    idle
    unwheeled <- settle 20
    when (P.pickerTop unwheeled /= 0) $
      fail ("selftest: the wheel back up left the finder's rows at " <> show (P.pickerTop unwheeled))

    -- A press on a row opens that row's file, as a press on the tree does.
    -- The rows start under the panel's title bar and its prompt, a row every
    -- line height, so the fourth of them is the third numbered file.
    click 300 185
    idle
    clicked <- appPath <$> readIORef ref
    unless (maybe False (equalFilePath (treeDir </> "file-03.txt")) clicked) $
      fail ("selftest: a press on the fourth row opened " <> show clicked)

    -- Escape puts the finder away with nothing picked.
    chord 'p'
    idle
    key plain KeyEscape
    idle
    escaped <- pickerNow
    when (isJust escaped) (fail "selftest: Escape did not put the finder away")

    -- The live grep, over the same folder, where there is a ripgrep to run.
    -- Ctrl+Shift+F puts the finder up over the lines of the files. A query is
    -- run only once typing has paused, so the frames go round until the
    -- prompt has settled and ripgrep has answered.
    findExecutable "rg" >>= \case
      Nothing -> say "skip: no rg on the PATH, so the live grep is not run"
      Just _ -> do
        frame base {inputChars = "f", inputModifiers = Modifiers True True False} >> idle
        let answered :: Int -> IO P.Picker
            answered 0 = fail "selftest: the grep never answered"
            answered k =
              idle >> pickerNow >>= \case
                Just pk | P.pickerDone pk && P.pickerCount pk > 0 -> pure pk
                Just _ -> threadDelay 20000 >> answered (k - 1)
                Nothing -> fail "selftest: Ctrl+Shift+F did not put the grep up"
        typed "PUTSTRLN"
        grepped <- answered 200
        when (P.pickerCount grepped /= 1) $
          fail ("selftest: \"PUTSTRLN\" grepped " <> show (P.pickerCount grepped) <> " lines, not 1")
        unless (fmap P.itemLine (P.pickerCurrent grepped) == Just (Just 4)) $
          fail ("selftest: the grep landed on line " <> show (P.itemLine <$> P.pickerCurrent grepped))
        shot "15-grep.bmp"
        -- Enter opens the file with the caret on the line that was found.
        modifyIORef' ref $ \a ->
          a {appEditor = (appEditor a) {edBuffer = B.markSaved (edBuffer (appEditor a))}}
        key plain KeyEnter
        idle
        landed <- readIORef ref
        let buf = edBuffer (appEditor landed)
        unless (maybe False (equalFilePath (treeDir </> "demo.hs")) (appPath landed) && B.lineOf buf (B.bufCursor buf) == 4) $
          fail ("selftest: the grep opened " <> show (appPath landed) <> " at line " <> show (B.lineOf buf (B.bufCursor buf)))


    -- Resize the window a step at a time, as a drag of its border does, and
    -- time the frames; then the same under a view of one label, for what the
    -- toolkit itself spends on a new size.
    let resizeRun name ui = do
          t0 <- getMonotonicTime
          forM_ [1 :: Int .. 100] $ \i -> do
            setWindowSize env (Size (1500 + 8 * fromIntegral i) (900 + 4 * fromIntegral i))
            (ctx', inp') <- syncDisplay ctx env base
            when (i == 100) (say ("  window is now " <> show (inputWindowSize inp')))
            void (sdlDrawFrame ctx' ui env inp' True)
          t1 <- getMonotonicTime
          say (printf "100 resized frames, %s: %.1f ms (%.2f ms a frame)" (name :: String) ((t1 - t0) * 1000) ((t1 - t0) * 10))
    resizeRun "editor" (appView ref)
    resizeRun "one label" (label "resize")
    say "ned selftest: ok"
