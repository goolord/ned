-- | Drives the application in a hidden window on scripted input, checks what
-- the editing did, and writes screenshots of it. Run with
-- @ned --selftest DIR@.
--
-- The finder's own thread is the one thing here that runs on the clock rather
-- than on frames, so the part that tests it waits on it between frames.
module Ned.Selftest (selftest) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, unless, void, when)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.NanoRope.Measured as Rope
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend (lineWidthIO, textInputArea)
import NanoUI.Backend.Sdl
import NanoUI.Input (emptyInput, inputKeysFromList)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Markdown (parseMarkdown)
import NanoUI.Testing (cursorKindIs, needsRedraw, newPixelContext, uiCursorKind)
import Ned.App
import Ned.App.State (Bar (..), Doc (..), Tip (..), WindowLayout (..), activeDoc, appDocs, closeDoc, docName, everyEditor, hoverAt, paneDocs, selectDoc)
import qualified Ned.Buffer as B
import Ned.Complete (Candidate (..), Completion (..))
import Ned.Config (Config (..), FileSettings (..), defaultConfig)
import Ned.Editor (Editor (..), cellWidth, defaultFontSize)
import Ned.Editor.Vim (Mode (..), Vim (..), newVim)
import qualified Ned.FileTree as FT
import Ned.Lsp (Diagnostic (..))
import qualified Ned.Picker as P
import Ned.View (appView)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, listDirectory, makeAbsolute, removeFile)
import System.Exit (exitFailure)
import System.FilePath (equalFilePath, takeDirectory, takeFileName, (</>))
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

selftestIn :: FilePath -> Maybe FilePath -> (String -> IO ()) -> IO ()
selftestIn dir mfile say = do
  ctx0 <- newPixelContext >>= (`withTheme` tomorrowNightMinDarkTheme)
  -- No language servers: what the test does is the same whatever is installed.
  blankApp <- newApp defaultConfig {cfgFiles = (cfgFiles defaultConfig) {fsLanguageServers = []}}
  tLoad0 <- getMonotonicTime
  -- The self-test types into the editor as it is without vim's keys, and
  -- every tab opened takes after the first.
  app0 <- maybe pure (openPath InNewTab Nothing) mfile blankApp {appEditor = (appEditor blankApp) {edVim = Nothing}}
  tLoad1 <- B.lineCount (edBuffer (appEditor app0)) `seq` getMonotonicTime
  say (printf "loaded %d lines in %.1f ms" (B.lineCount (edBuffer (appEditor app0))) ((tLoad1 - tLoad0) * 1000))
  ref <- newIORef app0
  let size = Size 1100 760
  withSdl defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsMode = Hidden, wsSize = size, wsResizable = False}, sdlAppVsync = False} ctx0 $ \ctx env -> do
    let base = emptyInput {inputWindowSize = size, inputMousePos = V2 600 400}
        frame inp = void (sdlDrawFrame ctx (appView ref) env inp False)
        idle = frame base >> frame base
        typed t = frame base {inputChars = t} >> idle
        -- A chord types nothing: it is the key, pressed with Ctrl held.
        chord c = key ctrlM (KeyChar c)
        key mods k = frame base {inputKeys = inputKeysFromList [k], inputKeysNew = inputKeysFromList [k], inputModifiers = mods} >> idle
        plain = noModifiers
        ctrlM = noModifiers {modCtrl = True}
        shiftM = noModifiers {modShift = True}
        ctrlShiftM = noModifiers {modCtrl = True, modShift = True}
        leftButton = buttonsFromList [MouseLeft]
        rightButton = buttonsFromList [MouseRight]
        at x y = base {inputMousePos = V2 x y}
        -- A click is a press and, a frame later, the release.
        tap x y = do
          frame (at x y) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton}
          frame (at x y) {inputButtonsReleased = leftButton}
        click x y = tap x y >> idle
        -- A press, a move with the button held, and the release there.
        drag x0 y0 x1 y1 = do
          frame (at x0 y0) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton}
          frame (at x1 y1) {inputButtonsHeld = leftButton}
          frame (at x1 y1) {inputButtonsReleased = leftButton}
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
    -- The window hands over typed text only while a widget asks for it, and
    -- the text, which has the keyboard, has to.
    asking <- textInputArea ctx
    unless (isJust asking) (fail "selftest: the text has the keyboard and asks for no typed text")

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
      cellW <- cellWidth (lineWidthIO fm)
      let run = T.replicate 150 "e"
      drawn <- lineWidthIO fm run
      when (abs (drawn - 150 * cellW) > 1) $
        fail (printf "selftest: at size %.1f a run of 150 cells is %.2f wide and is drawn %.2f wide" pt (150 * cellW) drawn)

    case mfile of
      Just _ -> do
        -- A press on the number of the last line in view takes that line, and
        -- leaves the view where it is: the caret it puts on the line after
        -- is nothing to scroll to.
        click 20 712
        scrolled <- edScrollY . appEditor <$> readIORef ref
        when (scrolled /= 0) $ fail ("selftest: a press on the last line number in view scrolled to " <> show scrolled)
        -- Scroll a loaded file about and time the frames.
        t0 <- getMonotonicTime
        forM_ [1 :: Int .. 200] $ \_ -> frame base {inputScroll = V2 0 1}
        t1 <- getMonotonicTime
        say (printf "200 scrolled frames in %.1f ms (%.2f ms a frame)" ((t1 - t0) * 1000) ((t1 - t0) * 5))
        shot "02-scrolled.bmp"
        key ctrlM KeyEnd
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
        key shiftM KeyHome
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
        key shiftM KeyTab
        expect "unindent selection" "main :: IO ()\nmain = do\n    putStrLn \"hello\" -- greet\n    ok"
        shot "04-selected.bmp"

        -- A click places the caret; a drag selects.
        click 300 300
        dirty <- B.isDirty . edBuffer . appEditor <$> readIORef ref
        unless dirty (fail "selftest: buffer should be dirty")
        shot "05-clicked.bmp"

        -- A press on a line number selects the line; dragging down takes more.
        let selection = B.selectedText . edBuffer . appEditor <$> readIORef ref
        frame (at 20 55) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton}
        one <- selection
        when (one /= "main = do" <> T.singleton (toEnum 10)) $ fail ("selftest: gutter press selected " <> show one)
        frame (at 20 75) {inputButtonsHeld = leftButton}
        frame (at 20 75) {inputButtonsReleased = leftButton}
        two <- selection
        when (length (T.lines two) /= 2) $ fail ("selftest: gutter drag selected " <> show two)
        shot "06-gutter.bmp"

        -- A double click takes the word and a triple click the line, and both
        -- keep them through the frames that hold the button, which report
        -- one click as the session's frames do. "putStrLn" is at 85..150.
        let press n x y = frame (at x y) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton, inputMouseClicks = n}
            hold x y = frame (at x y) {inputButtonsHeld = leftButton}
            release x y = frame (at x y) {inputButtonsReleased = leftButton} >> idle
        let putY = 73
        press 2 100 putY >> hold 100 putY >> hold 100 putY >> release 100 putY
        word <- selection
        when (word /= "putStrLn") $ fail ("selftest: double click selected " <> show word)
        press 2 100 putY >> hold 100 putY >> hold 180 putY >> release 180 putY
        words2 <- selection
        when (words2 /= "putStrLn " <> T.init (T.pack (show ("hello" :: String)))) $ fail ("selftest: double click and drag selected " <> show words2)
        press 3 100 putY >> hold 100 putY >> hold 100 putY >> release 100 putY
        line <- selection
        when (line /= "    putStrLn " <> T.pack (show ("hello" :: String)) <> " -- greet" <> T.singleton (toEnum 10)) $
          fail ("selftest: triple click selected " <> show line)

        -- A line wider than the view is what the sideways bar along the foot
        -- is for. Its thumb, held and dragged, walks the view along the line,
        -- and the button coming up leaves it where it was let go. Home takes
        -- the caret to the line's head, and the view along with it.
        key plain KeyEnd
        key plain KeyEnter
        typed (T.replicate 400 "x")
        key plain KeyHome
        idle
        still <- edScrollX . appEditor <$> readIORef ref
        when (still /= 0) $ fail ("selftest: a long line left the view sideways at " <> show still)
        drag 600 721 1000 721
        idle
        draggedX <- edScrollX . appEditor <$> readIORef ref
        when (draggedX <= 0) $
          fail ("selftest: dragging the sideways thumb left the view at " <> show draggedX)
        frame (at 300 400)
        idle
        letGo <- edScrollX . appEditor <$> readIORef ref
        when (letGo /= draggedX) $
          fail "selftest: the sideways thumb kept following the pointer after the button came up"
        snap "06b-sideways.bmp"
        key plain KeyHome
        idle
        backX <- edScrollX . appEditor <$> readIORef ref
        when (backX /= 0) $ fail ("selftest: Home left the view sideways at " <> show backX)

        -- The bound is the file's, not the view's: with lines put under the
        -- long one so it is walked out of sight, the bar stays and the view
        -- still goes to it.
        forM_ [1 :: Int .. 40] (const (key plain KeyEnter))
        key shiftM KeyHome
        drag 600 721 1000 721
        idle
        reached <- edScrollX . appEditor <$> readIORef ref
        when (reached <= 0) $
          fail ("selftest: with the long line out of view the sideways bar dragged to " <> show reached)
        key shiftM KeyHome
        idle
        backTop <- edScrollX . appEditor <$> readIORef ref
        when (backTop /= 0) $ fail ("selftest: Ctrl+Home left the view sideways at " <> show backTop)

        -- What is under the pointer says what the cursor is: the text has the
        -- beam, the scrollbars and the line numbers the arrow.
        let crossing name x y want = do
              frame (at x y) >> frame (at x y)
              kind <- uiCursorKind ctx (at x y)
              when (kind /= want) $ fail ("selftest: cursor " <> show kind <> " " <> name)
        crossing "on the scrollbar" 1094 400 UiCursorDefault
        crossing "on the sideways bar" 600 721 UiCursorDefault
        crossing "on the text" 600 400 UiCursorText
        crossing "on the line numbers" 20 400 UiCursorDefault

        -- A right click outside the selection moves the caret and opens the menu.
        frame (at 300 40) {inputButtonsHeld = rightButton, inputButtonsPressed = rightButton}
        frame (at 300 40) {inputButtonsReleased = rightButton}
        none <- selection
        unless (T.null none) $ fail "selftest: right click kept the selection"
        frame (at 300 40)
        snap "07-context-menu.bmp"
        tap 900 500

        -- A click on a menu's button has to ask for the frame that shows the
        -- menu: nothing else will, with the pointer at rest.
        _ <- sdlDrawFrame ctx (appView ref) env (at 17 13) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton} False
        dirty1 <- sdlDrawFrame ctx (appView ref) env (at 17 13) {inputButtonsReleased = leftButton} False
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
    -- A file the tree opens takes the place of the one in front, which asks
    -- first when that has changes; the ones typed here are put down.
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
    -- In the tab that was in front, in place of what it held.
    let tabNames = map docName . appDocs <$> readIORef ref
    inPlace <- tabNames
    when (inPlace /= ["inner.txt"]) $
      fail ("selftest: after the tree opened a file the tabs were " <> show inPlace)
    typed "x"
    typedInto <- text
    when (typedInto == opened) $ fail "selftest: the text took nothing after the tree opened it"

    -- A shift-click on a file in the tree opens it in a tab of its own, and
    -- leaves the one with changes alone without asking. The rows start under
    -- the menu bar and the tree's own heading, a row every 22.
    frame (at 60 114) {inputModifiers = shiftM, inputButtonsHeld = leftButton, inputButtonsPressed = leftButton}
    frame (at 60 114) {inputModifiers = shiftM, inputButtonsReleased = leftButton}
    idle
    newTab <- tabNames
    shiftAsked <- appPending <$> readIORef ref
    when (newTab /= ["inner.txt", "outer.txt"] || isJust shiftAsked) $
      fail ("selftest: a shift-click in the tree left the tabs at " <> show newTab)
    -- A plain click on a file that is open brings its tab forward, changes
    -- and all, rather than asking to put it in the place of this one.
    click 60 92
    backTo <- appPath <$> readIORef ref
    plainAsked <- appPending <$> readIORef ref
    unless (maybe False (equalFilePath (treeDir </> "sub" </> "inner.txt")) backTo && not (isJust plainAsked)) $
      fail ("selftest: a click on an open file in the tree brought " <> show backTo <> " forward")

    -- Every row of the tree is the one widget, so a pointer that moves from
    -- one row to the next crosses onto no other widget. The tree asks for a
    -- frame on every move over it, or the row drawn under the pointer would
    -- wait for whatever wants the next frame, which between caret blinks is
    -- half a second. The text asks for none: nothing it draws follows a
    -- pointer that is only passing over it. What the pointer passed on the
    -- way fades out first, so that a frame wanted for a fade is not taken
    -- for one wanted for the move.
    let quiet inp n = do
          frame inp {inputDeltaTime = 0.05}
          busy <- needsRedraw ctx inp inp
          when (busy && n > (0 :: Int)) (quiet inp (n - 1))
        over = base {inputMousePos = V2 60 70}
        away = base {inputMousePos = V2 900 400}
    quiet over 100
    overNeeds <- needsRedraw ctx over over {inputMousePos = V2 60 73}
    unless overNeeds (fail "selftest: a pointer moving over the tree asked for no frame")
    quiet away 100
    awayNeeds <- needsRedraw ctx away away {inputMousePos = V2 903 400}
    when awayNeeds (fail "selftest: a pointer moving over the text asked for a frame")

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
            Just pk | P.pkDone pk -> pure pk
            Just _ -> threadDelay 20000 >> settle (k - 1)
            Nothing -> fail "selftest: Ctrl+P did not put the finder up"
        landedOn what name pk =
          unless (maybe False ((== name) . P.itemText) (P.currentItem pk)) $
            fail ("selftest: " <> what <> " landed on " <> show (P.itemText <$> P.currentItem pk))
        clearQuery n = forM_ [1 .. n :: Int] (\_ -> key plain KeyBackspace) >> idle
    gathered <- settle 200
    -- The folder holds 60 numbered files, outer.txt, and inner.txt inside sub.
    when (P.pkTaken gathered /= 62) $
      fail ("selftest: the finder found " <> show (P.pkTaken gathered) <> " files, not 62")
    shot "11-picker.bmp"

    -- A query narrows the rows, and the row the keyboard is on is previewed.
    typed "inner"
    idle
    narrowed <- settle 20
    when (P.hitCount narrowed /= 1) $
      fail ("selftest: \"inner\" matched " <> show (P.hitCount narrowed) <> " files, not 1")
    landedOn "the query \"inner\"" "sub/inner.txt" narrowed
    shot "12-picker-query.bmp"

    -- Enter opens what the keyboard is on and puts the finder away. The file
    -- is open in a tab already, from the tree, and that tab is what comes to
    -- the front, with what was typed into it.
    tabsBefore <- length . appDocs <$> readIORef ref
    key plain KeyEnter
    idle
    pickedPath <- appPath <$> readIORef ref
    unless (maybe False (equalFilePath (treeDir </> "sub" </> "inner.txt")) pickedPath) $
      fail ("selftest: the finder opened " <> show pickedPath)
    tabsAfter <- length . appDocs <$> readIORef ref
    when (tabsAfter /= tabsBefore) $
      fail ("selftest: opening a file that was open made " <> show tabsAfter <> " tabs of " <> show tabsBefore)
    reopened <- text
    when (reopened == "inner\n") $ fail "selftest: opening a file that was open lost what was typed into it"
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
    -- An empty query keeps the rows in the order the walk found them, which
    -- is the order the platform lists the directory in: the walk is
    -- dir-traverse's, and its order is the listing's.
    listed <- listDirectory treeDir >>= filterM (doesFileExist . (treeDir </>))
    clearQuery 4
    cleared <- settle 20
    when (P.hitCount cleared /= 63) $
      fail ("selftest: an empty query kept " <> show (P.hitCount cleared) <> " rows, not 63")
    landedOn "an emptied query" (T.pack (listed !! 0)) cleared
    key plain KeyDown
    key plain KeyDown
    chord 'n'
    walked <- settle 20
    landedOn "three steps down the rows" (T.pack (listed !! 3)) walked
    chord 'p'
    stepped <- settle 20
    landedOn "a step back up the rows" (T.pack (listed !! 2)) stepped

    -- The rows' scrollbar is down the right of them. Its thumb, held and
    -- dragged down, scrolls them; the button coming up lets go of it.
    drag 462 130 462 400
    idle
    dragged <- settle 20
    when (P.pkScroll dragged <= 0) $
      fail ("selftest: dragging the thumb left the finder's rows at " <> show (P.pkScroll dragged))
    frame (at 462 200)
    idle
    released <- settle 20
    when (P.pkScroll released /= P.pkScroll dragged) $
      fail "selftest: the thumb kept following the pointer after the button came up"

    -- The wheel over the rows scrolls them, and back up again.
    frame base {inputMousePos = V2 300 250}
    frame base {inputMousePos = V2 300 250, inputScroll = V2 0 4}
    idle
    wheeled <- settle 20
    when (P.pkScroll wheeled <= 0) $
      fail ("selftest: the wheel left the finder's rows at " <> show (P.pkScroll wheeled))
    frame base {inputMousePos = V2 300 250, inputScroll = V2 0 (-40)}
    idle
    unwheeled <- settle 20
    when (P.pkScroll unwheeled /= 0) $
      fail ("selftest: the wheel back up left the finder's rows at " <> show (P.pkScroll unwheeled))

    -- A press on a row opens that row's file, as a press on the tree does.
    -- Which row a y of the window lands on is the font's business, so the
    -- row is read back from where the finder says the pointer is.
    frame (at 300 185)
    hoveredRow <- pickerNow >>= maybe (fail "selftest: the rows took no frame under the pointer") (pure . P.pkHovered)
    when (hoveredRow < 0) $ fail "selftest: the pointer over the rows hovered no row"
    click 300 185
    idle
    clicked <- appPath <$> readIORef ref
    unless (maybe False (equalFilePath (treeDir </> (listed !! hoveredRow))) clicked) $
      fail ("selftest: a press on the row the pointer was on, row " <> show hoveredRow <> ", opened " <> show clicked)

    -- The button at the end of the prompt empties it, and every row answers
    -- again. The prompt keeps the keyboard, so typing carries on in it.
    chord 'p'
    _ <- settle 200
    typed "outer"
    narrowedAgain <- settle 20
    when (P.hitCount narrowedAgain /= 1) $
      fail ("selftest: \"outer\" matched " <> show (P.hitCount narrowedAgain) <> " files, not 1")
    click 451 69
    emptiedPrompt <- settle 20
    unless (T.null (P.pkTyped emptiedPrompt) && P.hitCount emptiedPrompt == 63) $
      fail ("selftest: the clear button left " <> show (P.pkTyped emptiedPrompt) <> " and " <> show (P.hitCount emptiedPrompt) <> " rows")
    typed "in"
    typedOn <- settle 20
    unless (P.pkTyped typedOn == "in") $
      fail ("selftest: typing after the clear button left the prompt at " <> show (P.pkTyped typedOn))

    -- Escape puts the finder away with nothing picked.
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
        key ctrlShiftM (KeyChar 'f')
        let answered :: Int -> IO P.Picker
            answered 0 = fail "selftest: the grep never answered"
            answered k =
              idle >> pickerNow >>= \case
                Just pk | P.pkDone pk && P.hitCount pk > 0 -> pure pk
                Just _ -> threadDelay 20000 >> answered (k - 1)
                Nothing -> fail "selftest: Ctrl+Shift+F did not put the grep up"
        typed "PUTSTRLN"
        grepped <- answered 200
        when (P.hitCount grepped /= 1) $
          fail ("selftest: \"PUTSTRLN\" grepped " <> show (P.hitCount grepped) <> " lines, not 1")
        unless (fmap P.itemLine (P.currentItem grepped) == Just (Just 4)) $
          fail ("selftest: the grep landed on line " <> show (P.itemLine <$> P.currentItem grepped))
        shot "15-grep.bmp"
        -- A query nothing answers clears the rows the last query found, once
        -- ripgrep has finished with it and found nothing.
        let emptied :: Int -> IO P.Picker
            emptied 0 = fail "selftest: the unanswered grep never finished"
            emptied k =
              idle >> pickerNow >>= \case
                Just pk | P.pkDone pk && P.hitCount pk == 0 -> pure pk
                Just _ -> threadDelay 20000 >> emptied (k - 1)
                Nothing -> fail "selftest: the grep put itself away"
        typed "X"
        emptyResult <- emptied 200
        when (P.hitCount emptyResult /= 0) $
          fail ("selftest: an unanswered query left " <> show (P.hitCount emptyResult) <> " rows up")
        shot "16-grep-empty.bmp"
        key plain KeyBackspace
        _ <- answered 200
        -- Enter opens the file with the caret on the line that was found.
        key plain KeyEnter
        idle
        landed <- readIORef ref
        let buf = edBuffer (appEditor landed)
        unless (maybe False (equalFilePath (treeDir </> "demo.hs")) (appPath landed) && B.lineOf buf (B.bufCursor buf) == 4) $
          fail ("selftest: the grep opened " <> show (appPath landed) <> " at line " <> show (B.lineOf buf (B.bufCursor buf)))


    -- The tabs. Ctrl+N opens an untitled one after the one in front, which
    -- is put on the last for this, and brings it to the front.
    modifyIORef' ref (\a -> selectDoc (docKey (last (appDocs a))) a)
    idle
    let docsNow = appDocs <$> readIORef ref
        frontNow = appDocKey <$> readIORef ref
    before <- docsNow
    chord 'n'
    opened' <- docsNow
    newKey <- frontNow
    unless (length opened' == length before + 1 && docKey (last opened') == newKey) $
      fail ("selftest: Ctrl+N left the tabs at " <> show (map docName opened'))
    typed "draft"
    shot "17-tabs.bmp"

    -- Ctrl+Tab walks the tabs round from the last to the first, and
    -- Ctrl+Shift+Tab back again.
    key ctrlM KeyTab
    wrapped <- frontNow
    when ([wrapped] /= take 1 (map docKey opened')) $ fail "selftest: Ctrl+Tab on the last tab did not go round to the first"
    key ctrlShiftM KeyTab
    back' <- frontNow
    when (back' /= newKey) $ fail "selftest: Ctrl+Shift+Tab on the first tab did not go round to the last"

    -- Closing a tab with changes asks first; closing it once they are saved
    -- does not, and brings the tab before it to the front.
    chord 'w'
    asked <- appPending <$> readIORef ref
    unless (isJust asked) $ fail "selftest: Ctrl+W closed a tab with changes without asking"
    shot "18-close-asked.bmp"
    modifyIORef' ref $ \a ->
      a {appPending = Nothing, appEditor = (appEditor a) {edBuffer = B.markSaved (edBuffer (appEditor a))}}
    idle
    chord 'w'
    closed' <- docsNow
    front' <- frontNow
    unless (map docKey closed' == map docKey before && front' == docKey (last before)) $
      fail ("selftest: Ctrl+W left the tabs at " <> show (map docName closed'))

    -- A click on a tab brings it to the front. With the tree put away the
    -- tabs start at the window's left edge, under the title bar.
    chord 'b'
    click 60 57
    clickedTab <- frontNow
    when ([clickedTab] /= take 1 (map docKey before)) $ fail "selftest: a click on the first tab did not bring it to the front"
    chord 'b'
    idle

    -- A drag of a tab. One pane again, of three clean files, the tree at the
    -- left, so the places to press and to drop are known: the strip's first
    -- tab is a little right of the tree, the next a tab on from it.
    aTabs <- readIORef ref
    let stripped = foldl' (flip closeDoc) aTabs {appPanes = [], appTabDrag = Nothing} (map docKey (appDocs aTabs))
    replaced <- openPath InFrontTab Nothing (treeDir </> "outer.txt") stripped
    opened2 <- openPath InNewTab Nothing (treeDir </> "inner.txt") replaced
    opened3 <- openPath InNewTab Nothing (treeDir </> "demo.hs") opened2
    writeIORef ref opened3
    idle
    let stripNames = map docName . appDocs <$> readIORef ref
        dragTab x0 x1 y1 = do
          frame (at x0 48) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton}
          frame (at x1 y1) {inputButtonsHeld = leftButton}
          frame (at x1 y1) {inputButtonsHeld = leftButton, inputButtonsReleased = leftButton}
        paneCount = length . appPanes <$> readIORef ref
    order0 <- stripNames
    when (order0 /= ["outer.txt", "inner.txt", "demo.hs"]) $
      fail ("selftest: the drag tests were to start from three tabs, at " <> show order0)

    -- Across the strip, to its end: the order changes, and the tab dragged
    -- stays the one in front.
    dragTab 300 1000 48
    idle
    order1 <- stripNames
    when (order1 /= ["inner.txt", "demo.hs", "outer.txt"]) $
      fail ("selftest: dragging the first tab past the others left them at " <> show order1)
    frontPath <- appPath <$> readIORef ref
    unless (fmap takeFileName frontPath == Just "outer.txt") $
      fail ("selftest: dragging a tab left " <> show frontPath <> " in front")

    -- Down, off the strip, to the pane's edge: the pane splits, the tab
    -- dragged alone in the new pane, which is the one in front.
    frame (at 300 48) {inputButtonsHeld = leftButton, inputButtonsPressed = leftButton}
    frame (at 300 300) {inputButtonsHeld = leftButton}
    snap "21-tab-drag.bmp"
    frame (at 300 300) {inputButtonsHeld = leftButton, inputButtonsReleased = leftButton}
    idle
    splitPanes <- paneCount
    splitFront <- appPath <$> readIORef ref
    unless (splitPanes == 1) $
      fail (printf "selftest: dragging a tab off the strip left %d panes beside the one in front" splitPanes)
    unless (fmap takeFileName splitFront == Just "inner.txt") $
      fail ("selftest: the pane a drag split off shows " <> show splitFront)
    shot "22-split.bmp"

    -- Onto the other pane, below its strip: the tab moves to it, in front.
    dragTab 730 400 300
    idle
    movedPanes <- paneCount
    movedFront <- docName . activeDoc <$> readIORef ref
    movedBeside <- readIORef ref >>= \a -> case appPanes a of
      [p] -> pure (map docName (paneDocs p))
      other -> fail ("selftest: the row has " <> show (length other) <> " panes beside the one in front")
    unless (movedPanes == 1 && movedFront == "demo.hs" && movedBeside == ["outer.txt"]) $
      fail ("selftest: dropping a tab on the pane beside left " <> show movedFront <> " in front of " <> show movedBeside)
    shot "23-moved.bmp"

    -- The cross on the last tab of a pane takes the pane with it, the row
    -- closing up behind it.
    click 772 50
    idle
    closedPanes <- paneCount
    closedDocs <- stripNames
    unless (closedPanes == 0 && closedDocs == ["inner.txt", "demo.hs"]) $
      fail ("selftest: closing a pane's last tab left " <> show closedDocs <> ", with " <> show closedPanes <> " panes beside the one in front")
    shot "24-pane-closed.bmp"

    -- Completion, in a tab of its own: Tab after a word puts in the nearest
    -- word it starts and opens the menu on it, Ctrl+N steps down it and opens
    -- no file, Enter takes the word, and Ctrl+E puts back what was typed. The
    -- words of the other tabs are offered after the text's own.
    chord 'n'
    let menuNow = fmap (V.toList . cmShown) . edCompletion . appEditor <$> readIORef ref
        tabsNow = length . appDocs <$> readIORef ref
    tabsWere <- tabsNow
    typed "football foobar greeting"
    key plain KeyEnter
    typed "fo"
    key plain KeyTab
    expect "Tab completes the word before the caret" "football foobar greeting\nfootball"
    shot "18b-complete.bmp"
    chord 'n'
    expect "Ctrl+N steps down the menu" "football foobar greeting\nfoobar"
    tabsAre <- tabsNow
    when (tabsAre /= tabsWere) (fail "selftest: Ctrl+N opened a file while the menu was open")
    key plain KeyEnter
    expect "Enter takes the word, and breaks no line" "football foobar greeting\nfoobar"
    menuNow >>= \m -> when (isJust m) (fail "selftest: Enter left the menu open")
    typed " gre"
    key plain KeyTab
    menuNow >>= \case
      Just [Candidate "greeting" "", Candidate "greet" "demo.hs"] -> pure ()
      other -> fail ("selftest: the menu for gre is " <> show other)
    shot "18c-complete-sources.bmp"
    chord 'e'
    expect "Ctrl+E puts back what was typed" "football foobar greeting\nfoobar gre"
    menuNow >>= \m -> when (isJust m) (fail "selftest: Ctrl+E left the menu open")
    -- The names in a tags file above the tree's folder are offered too, once
    -- the watcher has found the file, which it looks for every two seconds.
    -- One name alone is put in with no menu.
    treeRoot <- FT.ftRoot . appTree <$> readIORef ref
    unless (equalFilePath treeRoot treeDir) (fail ("selftest: the tree is on " <> treeRoot <> " and not on " <> treeDir))
    writeFile (dir </> "tags") "!_TAG_FILE_SORTED\t1\t//\nzebraStripes\tzebra.c\t/^int zebraStripes;$/;\"\tv\n"
    forM_ [1 :: Int .. 25] (\_ -> idle >> threadDelay 100000)
    typed " zeb"
    key plain KeyTab
    expect "Tab completes a name in the tags file" "football foobar greeting\nfoobar gre zebraStripes"
    removeFile (dir </> "tags")

    -- With vim's keys, Tab completes in insert mode and Escape leaves it,
    -- closing the menu.
    modifyIORef' ref (everyEditor (\e -> e {edVim = Just newVim}))
    idle
    typed "Sfoo"
    key plain KeyTab
    expect "Tab completes in insert mode" "football foobar greeting\nfootball"
    key plain KeyEscape
    menuNow >>= \m -> when (isJust m) (fail "selftest: Escape left the menu open")
    vimMode' <- fmap vimMode . edVim . appEditor <$> readIORef ref
    unless (vimMode' == Just Normal) (fail ("selftest: Escape with the menu open left vim in " <> show vimMode'))
    -- Ctrl+N and Ctrl+P are vim's: they open the menu in insert mode as Tab
    -- does, and step through it; Ctrl+W deletes the word before the caret.
    -- None of them opens, finds or closes a file.
    typed "Sfoo"
    chord 'n'
    expect "Ctrl+N completes in insert mode" "football foobar greeting\nfootball"
    chord 'n'
    expect "Ctrl+N steps down the menu in insert mode" "football foobar greeting\nfoobar"
    chord 'p'
    expect "Ctrl+P steps up the menu in insert mode" "football foobar greeting\nfootball"
    key plain KeyEnter
    typed " zebra"
    chord 'w'
    expect "Ctrl+W deletes the word before the caret" "football foobar greeting\nfootball "
    key plain KeyEscape
    chord 'p'
    caretLine <- (\a -> let b = edBuffer (appEditor a) in B.lineOf b (B.bufCursor b)) <$> readIORef ref
    unless (caretLine == 0) (fail "selftest: Ctrl+P in normal mode did not move up a line")
    chord 'w'
    (,) <$> tabsNow <*> (isJust . appPicker <$> readIORef ref) >>= \case
      (n, False) | n == tabsWere -> pure ()
      other -> fail ("selftest: vim's Ctrl+N, Ctrl+P or Ctrl+W reached the application: " <> show other)
    modifyIORef' ref (everyEditor (\e -> e {edVim = Nothing}))

    -- Vim's keys, in a tab of their own: insert mode types, Escape steps back
    -- onto the text, normal mode edits it, and the leader puts the finder up
    -- a frame later without typing the keys that asked for it into it.
    chord 'n'
    modifyIORef' ref (everyEditor (\e -> e {edVim = Just newVim}))
    idle
    let vimNow = fmap vimMode . edVim . appEditor <$> readIORef ref
        caretNow = B.bufCursor . edBuffer . appEditor <$> readIORef ref
    typed "ihello world"
    expect "insert mode types" "hello world"
    key plain KeyEscape
    (,) <$> vimNow <*> caretNow >>= \case
      (Just Normal, 10) -> pure ()
      other -> fail ("selftest: Escape left vim at " <> show other)
    typed "0dw"
    expect "dw in normal mode" "world"
    -- Ctrl+H gives the keyboard to the tree beside the text, Ctrl+L gives it
    -- back.
    chord 'h'
    treeHas <- appTreeFocus <$> readIORef ref
    treeAsks <- textInputArea ctx
    unless (isJust treeAsks) (fail "selftest: the tree has the keyboard with vim's keys on and asks for no typed text")
    -- There vim's keys walk the tree: G and gg to the ends, l into a folder
    -- and h back out of it and closed, j and k with a count, and - up to the
    -- folder above, on the one it was.
    modifyIORef' ref (\a -> a {appTree = FT.collapseAll (appTree a)})
    let selectedName = fmap takeFileName . FT.ftSelected <$> treeNow
        expectSelected what want = do
          got <- selectedName
          when (got /= Just want) $
            fail ("selftest: vim's " <> what <> " in the tree selected " <> show got <> ", not " <> show want)
    typed "G"
    expectSelected "G" "outer.txt"
    typed "gg"
    expectSelected "gg" "sub"
    -- The folder holds the files the finder's test left in it as well.
    let expectFirstRows what want = do
          got <- take (length want) <$> names
          when (got /= want) $
            fail ("selftest: " <> what <> ": expected rows starting " <> show want <> ", got " <> show got)
    typed "l"
    expectFirstRows "vim's l on a folder" ["sub", "inner.txt", "file-01.txt"]
    typed "2j"
    expectSelected "2j" "file-01.txt"
    typed "k"
    expectSelected "k" "inner.txt"
    typed "hh"
    expectFirstRows "vim's h twice from inside a folder" ["sub", "file-01.txt"]
    expectSelected "h" "sub"
    typed "-"
    upRoot <- FT.ftRoot <$> treeNow
    unless (equalFilePath upRoot (takeDirectory treeDir)) (fail ("selftest: vim's - in the tree put the root on " <> show upRoot))
    expectSelected "-" "tree"
    modifyIORef' ref (\a -> a {appTree = FT.setRoot treeDir (appTree a)})
    idle
    chord 'l'
    textHas <- not . appTreeFocus <$> readIORef ref
    unless (treeHas && textHas) (fail "selftest: Ctrl+H and Ctrl+L did not move the keyboard to the tree and back")
    shot "19-vim.bmp"
    typed " ff"
    idle
    pickerNow >>= \case
      Just pk | T.null (P.pkTyped pk) -> pure ()
      Just pk -> fail ("selftest: the leader's keys were typed into the finder: " <> show (P.pkTyped pk))
      Nothing -> fail "selftest: SPC f f did not put the finder up"
    key plain KeyEscape

    -- * finds the word under the caret as a whole word, and puts the find
    -- bar up without taking the keyboard from the text, so n and N go on
    -- from it. / puts the bar up with the keyboard, and Enter gives it back.
    let findNow = (\a -> (B.bufCursor (edBuffer (appEditor a)), appBar a == BarFind, appBarFocus a)) <$> readIORef ref
        expectFind what want = findNow >>= \got -> when (got /= want) (fail ("selftest: " <> what <> ": expected " <> show want <> ", got " <> show got))
    typed "Sfoo foobar foo"
    key plain KeyEscape
    typed "0*"
    idle
    expectFind "* went to the next whole word" (11, True, False)
    typed "n"
    idle
    expectFind "n went around to the first" (0, True, False)
    typed "/"
    idle
    expectFind "/ gave the bar the keyboard" (0, True, True)
    key plain KeyEnter
    expectFind "Enter found foo inside foobar, and gave the keyboard back" (4, True, False)
    typed "n"
    idle
    expectFind "n after Enter" (11, True, False)
    typed "N"
    idle
    expectFind "N after Enter" (4, True, False)
    shot "20-vim-search.bmp"
    typed ":noh"
    key plain KeyEnter
    expectFind ":noh put the bar away" (4, False, False)

    -- The command line: :tabnew opens a tab, :enew empties it, :e opens a
    -- file in it and :e! reads it again, and :%s replaces over every line.
    let command t = typed (":" <> t) >> key plain KeyEnter
        pathNow = appPath <$> readIORef ref
    exFile <- makeAbsolute (dir </> "ex.txt")
    writeFile exFile "from disk\nand disk\n"
    tabsAtCommand <- tabsNow
    command "tabnew"
    tabsNow >>= \n -> unless (n == tabsAtCommand + 1) (fail "selftest: :tabnew did not open a tab")
    typed "ichanged"
    key plain KeyEscape
    command "enew"
    isJust . appPending <$> readIORef ref >>= \up -> unless up (fail "selftest: :enew did not ask about the changes")
    modifyIORef' ref (\a -> a {appPending = Nothing})
    command "enew!"
    expect ":enew! throws the changes away" ""
    command ("e " <> T.pack exFile)
    expect ":e opens a file" "from disk\nand disk\n"
    pathNow >>= \p -> unless (fmap (equalFilePath exFile) p == Just True) (fail ("selftest: :e left the tab on " <> show p))
    command "%s/disk/memory/"
    expect ":%s replaces on every line" "from memory\nand memory\n"
    command "e!"
    expect ":e! reads the file again" "from disk\nand disk\n"
    command "q"
    tabsNow >>= \n -> unless (n == tabsAtCommand) (fail "selftest: :q did not close the tab")
    removeFile exFile
    modifyIORef' ref (everyEditor (\e -> e {edVim = Nothing}))

    -- What a language server says of a name, put up as though one had: code
    -- in a fence that names its language, and in one that names none, which
    -- takes the file's.
    modifyIORef' ref $ \a ->
      a {appHover = Just (hoverAt a, TipDoc $ parseMarkdown "```haskell\ngreeting :: Text -> IO ()\n```\n\nSays hello, `greeting \"you\"`.\n\n```\nmain = greeting \"world\" -- plain text\n```")}
    idle
    shot "19b-hover.bmp"
    modifyIORef' ref (\a -> a {appHover = Nothing})

    -- What a server found wrong, as a jump to it puts it up: an error whose
    -- message quotes code, and a warning at the same place.
    modifyIORef' ref $ \a ->
      a
        { appHover =
            Just
              ( hoverAt a
              , TipDiagnostics
                  [ Diagnostic (0, 0) (0, 4) 1 "\8226 Couldn't match expected type \8216Int\8217 with actual type \8216Text\8217\n\8226 In the first argument of \8216show\8217\n  |\n3 | main = print (show greeting)\n  |                     ^^^^^^^^"
                  , Diagnostic (0, 0) (0, 4) 2 "Defined but not used: \8216greeting\8217"
                  ]
              )
        }
    idle
    shot "19c-diagnostic.bmp"
    modifyIORef' ref (\a -> a {appHover = Nothing})

    -- The settings file under the window is watched: what an edit to it
    -- changes is taken up by a frame soon after, and a file that will not
    -- read changes nothing.
    let cfgFile = dir </> "config.dhall"
        readings = appConfigSeen <$> readIORef ref
        -- Frames until the watcher has read the file this many times.
        awaitReading n = go (30 :: Int)
          where
            go 0 = fail ("selftest: the settings were not read again " <> show n <> " times")
            go k = idle >> readings >>= \seen -> unless (seen >= n) (threadDelay 100000 >> go (k - 1))
    writeFile cfgFile "{=}"
    modifyIORef' ref (\a -> everyEditor (\e -> e {edShowWhitespace = True}) a {appConfigPath = Just cfgFile})
    idle
    threadDelay 700000
    writeFile cfgFile "{ bufferFontSize = 20.0, showIndentation = False }"
    awaitReading 1
    reloaded <- (\a -> (edFontSize (appEditor a), edShowWhitespace (appEditor a))) <$> readIORef ref
    unless (reloaded == (20, False)) (fail ("selftest: the reloaded settings left the text at " <> show reloaded))
    shot "20-reloaded.bmp"
    say "  a settings error follows, on purpose:"
    writeFile cfgFile "{ bufferFontSize = 30 }"
    awaitReading 2
    kept <- edFontSize . appEditor <$> readIORef ref
    unless (kept == 20) (fail ("selftest: a settings file that does not read changed the text size to " <> show kept))
    modifyIORef' ref (\a -> a {appConfigPath = Nothing})
    idle

    -- The outer pane grid's arrangement goes into the saved window layout. A
    -- drag of the tree pane onto the editor swaps which side it occupies.
    drag 60 55 800 400
    idle
    outerGrid <- layoutOuterGrid . appLayout <$> readIORef ref
    case outerGrid of
      Just (Split _ AxisV _ (Pane 2) (Pane 1)) -> pure ()
      other -> fail ("selftest: dragging the tree to the other side left the outer grid at " <> show other)

    -- Resize the window a step at a time, as a drag of its border does, and
    -- time the frames; then the same under a view of one label, for what the
    -- toolkit itself spends on a new size. Each frame asks for the size the
    -- next is drawn at, as a view asks the window for one.
    let resizeRun name ui = do
          t0 <- getMonotonicTime
          forM_ [1 :: Int .. 100] $ \i -> do
            (ctx', inp') <- syncDisplay ctx env base
            when (i == 100) (say ("  window is now " <> show (inputWindowSize inp')))
            void (sdlDrawFrame ctx' (ui >> resizeWindowUi (Size (1500 + 8 * fromIntegral i) (900 + 4 * fromIntegral i))) env inp' True)
          t1 <- getMonotonicTime
          say (printf "100 resized frames, %s: %.1f ms (%.2f ms a frame)" (name :: String) ((t1 - t0) * 1000) ((t1 - t0) * 10))
    resizeRun "editor" (appView ref)
    resizeRun "one label" (label "resize")
    say "ned selftest: ok"
