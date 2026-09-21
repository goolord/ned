-- | The fuzzy finder: a prompt, the rows that answer it, and a preview of the
-- one the keyboard is on.
--
-- Ctrl+P puts it up over the window. What it is looking through is a 'Source',
-- which gathers its rows on a thread of its own and feeds them over as it
-- finds them, so a picker over a hundred thousand files is usable from its
-- first frame and the window never waits on a disk. 'fileSource' is the one
-- the editor has: every file under the tree's root. A source that answers the
-- query itself rather than leaving it to the matcher -- a live grep -- is the
-- same record with 'srcLive' set, and nothing else here changes.
--
-- Scoring is fzf's own, through "Ned.Fuzzy": one call scores the whole
-- list, and the rows on screen ask again for which of their characters the
-- query matched, which is what the finder colours them by.
--
-- The whole of it is one module, state and frame and drawing together, so
-- that what a key does and what it looks like when it happens sit on the same
-- page. The rows and the preview are set in the editor's monospace font
-- rather than the chrome's: a path and a line of code are read on the grid,
-- and on the grid a matched character is exactly one cell wide, which is what
-- lets the matching be shown on the characters themselves.
module Ned.Picker
  ( -- * The finder
    Picker
  , openPicker
  , closePicker
  , pickerOverlay
  , pickerSig
  , pickerCount
  , pickerGathered
  , pickerDone
  , pickerCurrent
  , pickerTop

    -- * What it looks through
  , Item (..)
  , Sink
  , Source (..)
  , fileSource
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Effectful (Eff, type (:>))
import NanoUI
import NanoUI.Input (foldInputKeys)
import NanoUI.Context (Context (..), getPrevRect)
import NanoUI.Monad (askContext, askInput)
import Ned.Editor (cellWidth, defaultFontSize)
import qualified Ned.Fuzzy as Fuzzy
import Ned.Highlight
import Ned.Text (cellsAt, clamp)
import Ned.Theme
import Ned.Widget
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (makeRelative, (</>))
import System.IO (IOMode (ReadMode), withBinaryFile)

--------------------------------------------------------------------------------
-- What the finder looks through
--------------------------------------------------------------------------------

-- | One row: the text it is drawn and matched by, and what picking it opens.
data Item = Item
  { itemText :: !Text
  -- ^ What the query is matched against, and what the row shows.
  , itemPath :: !FilePath
  , itemLine :: !(Maybe Int)
  -- ^ The line the row is about, counted from zero. A file has none; a grep
  -- hit is the line it was found on, which is where the preview opens.
  }

-- | Where a gatherer hands its rows over, a batch at a time. It answers
-- 'False' when the picker has moved on -- another query, or the picker put
-- away -- and a gatherer that is told so stops.
type Sink = [Item] -> IO Bool

-- | Where a picker's rows come from.
--
-- The query reaches the gatherer as well as the matcher, and 'srcLive' says
-- which of the two answers it. A list of files is gathered once and the
-- matcher filters it on every keystroke; a grep is gathered again for every
-- query and shown in the order it comes back.
data Source = Source
  { srcTitle :: !Text
  -- ^ What the panel is called.
  , srcGather :: !(FilePath -> Text -> Sink -> IO ())
  -- ^ Find the rows under a root for a query, feeding them to the sink. It
  -- runs on a thread of its own, so it may take as long as it likes.
  , srcLive :: !Bool
  -- ^ Whether the query is the gatherer's to answer. A live source is
  -- gathered again whenever the query changes and is not filtered afterwards.
  }

-- | Every file under the root, nearest the root first.
fileSource :: Source
fileSource =
  Source
    { srcTitle = "Find File"
    , srcGather = \root _query sink -> walkFiles root sink
    , srcLive = False
    }

--------------------------------------------------------------------------------
-- Walking the files
--------------------------------------------------------------------------------

-- | How far down a walk goes, how many files it offers, and how many it
-- gathers before handing a batch over. The batch is what makes the rows
-- appear while the walk is still running; the other two are what keep a walk
-- that wandered somewhere enormous from running forever.
scanDepth, scanLimit, scanBatch :: Int
scanDepth = 24
scanLimit = 200000
scanBatch = 1024

-- | Directories a walk never goes into: what a version control system keeps
-- for itself, and what a build leaves behind. None of them holds a file
-- anybody opens, and all of them hold thousands.
--
-- The rest of a dotted directory is walked, so that the workflows under
-- @.github@ are found like anything else.
skipDir :: FilePath -> Bool
skipDir name =
  map toLower name
    `elem` [ ".git"
           , ".hg"
           , ".svn"
           , ".stack-work"
           , ".direnv"
           , ".cache"
           , ".mypy_cache"
           , ".pytest_cache"
           , ".ruff_cache"
           , ".venv"
           , ".gradle"
           , "dist-newstyle"
           , "node_modules"
           , "__pycache__"
           , "target"
           ]

-- | The files under a root, breadth first, handed over in batches as they are
-- found. Breadth first so that what is near the root -- which is what is
-- usually wanted -- is there to pick before the walk has finished.
walkFiles :: FilePath -> Sink -> IO ()
walkFiles root feed = go (Seq.singleton (root, 0 :: Int)) 0 [] 0
  where
    go queue !found batch !held = case Seq.viewl queue of
      Seq.EmptyL -> void (flush batch)
      (dir, depth) Seq.:< rest -> do
        (dirs, files) <- readEntries dir
        let items = map item files
            found' = found + length items
            queue'
              | depth >= scanDepth = rest
              | otherwise = foldl' (\q d -> q Seq.|> (d, depth + 1)) rest dirs
            batch' = reverse items ++ batch
            held' = held + length items
        if held' >= scanBatch
          then do
            ok <- flush batch'
            when (ok && found' < scanLimit) (go queue' found' [] 0)
          else go queue' found' batch' held'

    item path = Item {itemText = relative root path, itemPath = path, itemLine = Nothing}

    flush [] = pure True
    flush batch = feed (reverse batch)

-- | A path as the rows show it: where it sits under the root, written with
-- forward slashes whatever the platform separates with, since that is how a
-- path is typed at the prompt.
relative :: FilePath -> FilePath -> Text
relative root path = T.replace "\\" "/" (T.pack (makeRelative root path))

-- | What a directory holds: the directories to walk on, and the files to
-- offer, each by name. A directory that cannot be read holds nothing.
--
-- A directory that is a link is not walked on. It leads somewhere that is
-- either under the root already, and would be listed twice, or outside it,
-- and is not what the finder was asked for; a link back up is neither, and
-- would not end.
readEntries :: FilePath -> IO ([FilePath], [FilePath])
readEntries dir = do
  names <- attempt [] (listDirectory dir)
  entries <- traverse classify (sortOn (map toLower) names)
  pure ([p | Just (p, True) <- entries], [p | Just (p, False) <- entries])
  where
    -- A directory to walk on, a file to offer, or neither: a directory that
    -- is skipped is not a file to offer in its place.
    classify name = do
      let path = dir </> name
      isDir <- attempt False (doesDirectoryExist path)
      if not isDir
        then pure (Just (path, False))
        else do
          link <- attempt False (pathIsSymbolicLink path)
          pure (if link || skipDir name then Nothing else Just (path, True))

-- | An answer from the file system, or a stand-in when it will not give one.
attempt :: a -> IO a -> IO a
attempt fallback act = either (\(_ :: SomeException) -> fallback) id <$> try act

--------------------------------------------------------------------------------
-- The picker
--------------------------------------------------------------------------------

-- | What a gatherer has handed over so far. The generation is which gather it
-- belongs to: a gatherer whose generation has moved on is answered 'False' by
-- its sink and stops.
data Gathered = Gathered
  { gatGen :: !Int
  , gatBatches :: ![[Item]]
  -- ^ Newest first, so a batch costs nothing to add.
  , gatCount :: !Int
  , gatDone :: !Bool
  }

-- | The finder, between frames.
data Picker = Picker
  { pkSource :: !Source
  , pkRoot :: !FilePath
  , pkQuery :: !Text
  , pkSlab :: !Fuzzy.Slab
  -- ^ The matcher's scratch space, which is the frame's alone to write.
  , pkPattern :: !Fuzzy.Pattern
  -- ^ The query as the matcher has it, kept so that the rows on screen can
  -- ask which of their characters it matched.
  , pkCell :: !(IORef Gathered)
  -- ^ Where the gathering thread leaves what it has found.
  , pkGen :: !Int
  , pkTaken :: !Int
  -- ^ How many gathered rows are in 'pkItems' already.
  , pkDone :: !Bool
  , pkItems :: !(V.Vector Item)
  , pkCands :: !Fuzzy.Candidates
  -- ^ The same rows, laid out for the matcher to scan in one call.
  , pkHits :: !Fuzzy.Matches
  -- ^ What matched, best first, as indices into 'pkItems'.
  , pkCursor :: !Int
  -- ^ Which hit the keyboard is on.
  , pkScroll :: !Double
  -- ^ The hit at the top of the list; its fraction is how far it is scrolled
  -- out of view.
  , pkHovered :: !Int
  , pkNameCells :: !Int
  -- ^ How many cells the rows' column of names is wide: the longest name
  -- the rows have shown since the query last changed. It only grows, so
  -- that the folders beside the names stay put while the list is scrolled.
  , pkGrab :: !(Maybe Float)
  -- ^ Where the scrollbar's thumb is held, below its top, while it is dragged.
  , pkPreview :: !Preview
  }

-- | The file beside the rows, as far as it is read.
data Preview = Preview
  { pvOf :: !(Maybe FilePath)
  -- ^ What it is of, so that a file is read once however long it is looked at.
  , pvLines :: !(SmallArray PreviewLine)
  , pvNote :: !Text
  -- ^ What is there instead of the lines: a file that could not be read, or
  -- one there is no point showing.
  , pvHit :: !(Maybe Int)
  , pvMore :: !Bool
  -- ^ Whether the file goes on past the lines that were read.
  , pvScroll :: !Double
  , pvVersion :: !Int
  -- ^ Bumped whenever the lines change, for the drawing's content key.
  }

-- | A line of the preview, lexed when the file was read rather than on every
-- frame.
data PreviewLine = PreviewLine !Text ![Span]

emptyPreview :: Preview
emptyPreview =
  Preview
    { pvOf = Nothing
    , pvLines = emptySmallArray
    , pvNote = ""
    , pvHit = Nothing
    , pvMore = False
    , pvScroll = 0
    , pvVersion = 0
    }

-- | Put a finder up over a root, and set its gatherer going.
openPicker :: Source -> FilePath -> IO Picker
openPicker source root = do
  slab <- Fuzzy.newSlab
  pattern_ <- Fuzzy.compile (Fuzzy.defaultQuery "")
  cell <- newIORef (Gathered 0 [] 0 False)
  gather
    Picker
      { pkSource = source
      , pkRoot = root
      , pkQuery = ""
      , pkSlab = slab
      , pkPattern = pattern_
      , pkCell = cell
      , pkGen = 0
      , pkTaken = 0
      , pkDone = False
      , pkItems = V.empty
      , pkCands = Fuzzy.candidates V.empty
      , pkHits = Fuzzy.noMatches
      , pkCursor = 0
      , pkScroll = 0
      , pkHovered = -1
      , pkGrab = Nothing
      , pkNameCells = 0
      , pkPreview = emptyPreview
      }

-- | Put the finder away, and with it the thread that is gathering for it: the
-- generation moves on, so the next batch it offers is refused and it stops.
closePicker :: Picker -> IO ()
closePicker pk = writeIORef (pkCell pk) (Gathered (pkGen pk + 1) [] 0 True)

-- | Start gathering for the query the picker holds, dropping whatever an
-- earlier gather had found.
gather :: Picker -> IO Picker
gather pk = do
  let gen = pkGen pk + 1
      cell = pkCell pk
  writeIORef cell (Gathered gen [] 0 False)
  _ <-
    forkIO $ do
      _ <- try (srcGather (pkSource pk) (pkRoot pk) (pkQuery pk) (batchSink cell gen)) :: IO (Either SomeException ())
      atomicModifyIORef' cell $ \g -> (if gatGen g == gen then g {gatDone = True} else g, ())
  pure
    pk
      { pkGen = gen
      , pkTaken = 0
      , pkDone = False
      , pkItems = V.empty
      , pkCands = Fuzzy.candidates V.empty
      , pkHits = Fuzzy.noMatches
      , pkCursor = 0
      , pkScroll = 0
      }

batchSink :: IORef Gathered -> Int -> Sink
batchSink cell gen batch = atomicModifyIORef' cell $ \g ->
  if gatGen g /= gen
    then (g, False)
    else (g {gatBatches = batch : gatBatches g, gatCount = gatCount g + length batch}, True)

-- | A number that changes when anything the finder shows does, which is what
-- tells the application that the frame it drew is no longer what the finder
-- says and another is wanted. A gatherer that found more rows between frames
-- changes it without anybody having touched a key.
pickerSig :: Maybe Picker -> Int
pickerSig Nothing = 0
pickerSig (Just pk) =
  contentHash
    [ hashText (pkQuery pk)
    , pkTaken pk
    , hitCount pk
    , pkCursor pk
    , round (pkScroll pk * 64)
    , pkHovered pk
    , fromEnum (pkDone pk)
    , pvVersion (pkPreview pk)
    , round (pvScroll (pkPreview pk) * 64)
    ]

--------------------------------------------------------------------------------
-- Taking in what was gathered, and matching it
--------------------------------------------------------------------------------

-- | How many rows the matcher is asked for. Everything that matched is
-- counted whatever this is; this is only how many of them can be scrolled
-- through.
matchLimit :: Int
matchLimit = 20000

-- | The hits as they stand.
hitCount :: Picker -> Int
hitCount = U.length . Fuzzy.matchedIndices . pkHits

-- | The item a hit is of.
hitItem :: Picker -> Int -> Maybe Item
hitItem pk i
  | i < 0 || i >= hitCount pk = Nothing
  | otherwise = pkItems pk V.!? U.unsafeIndex (Fuzzy.matchedIndices (pkHits pk)) i

-- | The item the keyboard is on.
currentItem :: Picker -> Maybe Item
currentItem pk = hitItem pk (pkCursor pk)

-- | How many rows answer the query as it stands.
pickerCount :: Picker -> Int
pickerCount = hitCount

-- | How many rows have been gathered, answering the query or not.
pickerGathered :: Picker -> Int
pickerGathered = pkTaken

-- | Whether the gatherer has found everything there is.
pickerDone :: Picker -> Bool
pickerDone = pkDone

-- | How far down the rows are scrolled, in rows.
pickerTop :: Picker -> Double
pickerTop = pkScroll

-- | The row the keyboard is on, which is what Enter would open.
pickerCurrent :: Picker -> Maybe Item
pickerCurrent = currentItem

-- | Take in whatever the gatherer has found since the last frame, and answer
-- the query against it.
--
-- The query is the matcher's when the source gathered everything at once, and
-- the gatherer's when the source answers it itself; a live source that is
-- handed a new query starts again rather than filtering what the old one
-- found.
restock :: Picker -> Text -> IO Picker
restock pk0 query
  | srcLive (pkSource pk0) && changed = gather pk0 {pkQuery = query} >>= harvest
  | otherwise = do
      pk <- harvest pk0 {pkQuery = query}
      if changed || pkTaken pk /= pkTaken pk0 then rematch changed pk else pure pk
  where
    changed = query /= pkQuery pk0

-- | Fold the batches the gatherer has left into the rows the matcher scans.
harvest :: Picker -> IO Picker
harvest pk = do
  g <- readIORef (pkCell pk)
  if gatGen g /= pkGen pk || (gatCount g == pkTaken pk && gatDone g == pkDone pk)
    then pure pk
    else do
      let items = V.fromList (concat (reverse (gatBatches g)))
      pure
        pk
          { pkItems = items
          , pkCands = Fuzzy.candidates (V.map itemText items)
          , pkTaken = gatCount g
          , pkDone = gatDone g
          }

-- | Score every row against the query and keep the best.
--
-- A new query puts the keyboard back on the first row, since the row it was
-- on has nothing to do with what is there now. Rows merely arriving from the
-- gatherer leave it where it was: they are added to the end of what an empty
-- query keeps, and a list that walked out from under the reader while it
-- filled would be unusable.
rematch :: Bool -> Picker -> IO Picker
rematch fresh pk = do
  pattern_ <- Fuzzy.compile (Fuzzy.defaultQuery (matchQuery pk))
  hits <- Fuzzy.matchCandidates (pkSlab pk) pattern_ (pkCands pk) matchLimit
  pure
    pk
      { pkPattern = pattern_
      , pkHits = hits
      , pkCursor = if fresh then 0 else min (pkCursor pk) (max 0 (U.length (Fuzzy.matchedIndices hits) - 1))
      , pkScroll = if fresh then 0 else pkScroll pk
      , pkNameCells = if fresh then 0 else pkNameCells pk
      }

-- | What the matcher is given. A live source has answered the query already,
-- so what comes back is shown in the order it came.
matchQuery :: Picker -> Text
matchQuery pk = if srcLive (pkSource pk) then "" else pkQuery pk

--------------------------------------------------------------------------------
-- The preview
--------------------------------------------------------------------------------

-- | How much of a file the preview reads, and how many of its lines it keeps.
-- A preview is looked at rather than read, and a file the editor would open
-- in a moment is not worth reading twice.
previewBytes, previewLines :: Int
previewBytes = 128 * 1024
previewLines = 600

-- | Read the file the keyboard is on, if it is not the one already read.
ensurePreview :: Picker -> IO Picker
ensurePreview pk = case currentItem pk of
  Nothing
    | not (isJust (pvOf (pkPreview pk))) -> pure pk
    | otherwise -> pure pk {pkPreview = emptyPreview {pvVersion = pvVersion (pkPreview pk) + 1}}
  Just item
    | pvOf (pkPreview pk) == Just (itemPath item) && pvHit (pkPreview pk) == itemLine item -> pure pk
    | otherwise -> do
        pv <- readPreview item
        pure pk {pkPreview = pv {pvVersion = pvVersion (pkPreview pk) + 1}}

-- | The head of a file, lexed for colour. Only the first 'previewBytes' are
-- read: what is past that is past what anybody previews, and reading it would
-- put a frame on hold for a file the reader may be arrowing straight past.
readPreview :: Item -> IO Preview
readPreview item = do
  raw <- try (withBinaryFile (itemPath item) ReadMode (\h -> BS.hGet h previewBytes))
  pure $ case raw of
    Left (_ :: SomeException) -> note "This file cannot be read."
    Right bytes
      | BS.null bytes -> note "This file is empty."
      | BS.elem 0 bytes -> note "This is not a text file."
      | otherwise ->
          blank
            { pvLines = smallArrayFromList (lexAll (languageFor (itemPath item)) ls)
            , pvMore = cut || length whole > previewLines
            }
      where
        text = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') bytes
        -- A file longer than the read ends mid-line, and half a line shown
        -- whole is a line that says something the file does not.
        cut = BS.length bytes >= previewBytes
        whole = dropLast cut (T.lines text)
        ls = take previewLines whole
  where
    -- A preview nobody has scrolled yet opens where the file is about, which
    -- the drawing works out once it knows how tall the pane is.
    blank = emptyPreview {pvOf = Just (itemPath item), pvHit = itemLine item, pvScroll = -1}
    note msg = blank {pvNote = msg}
    dropLast False xs = xs
    dropLast True xs = if null xs then xs else init xs

-- | The lines with what the lexer made of each, the state carrying from one
-- to the next as it does in the editor. Tabs are laid out before lexing, so
-- that a span's length is the cells it covers.
lexAll :: Lang -> [Text] -> [PreviewLine]
lexAll lang = go LexNormal
  where
    go _ [] = []
    go st (l : rest) =
      let t = expandTabs l
          (spans, st') = lexLine lang st t
       in PreviewLine t spans : go st' rest

-- | A line with its tabs laid out as the spaces they stand for.
expandTabs :: Text -> Text
expandTabs t
  | not (T.any (== '\t') t) = t
  | otherwise = T.pack (go 0 (T.unpack t))
  where
    go _ [] = []
    go !cell (c : cs) =
      let n = cellsAt cell c
       in (if c == '\t' then replicate n ' ' else [c]) ++ go (cell + n) cs

--------------------------------------------------------------------------------
-- One frame
--------------------------------------------------------------------------------

-- | Where the finder puts things. A row is scanned rather than read, so it
-- sits tighter than a line of the preview, which is code and is read.
pickerFontSize, pickerPad, pickerMark, pickerIcon :: Float
pickerFontSize = defaultFontSize
pickerPad = 8
pickerMark = 3
pickerIcon = 18

-- | The lane the rows' scrollbar has, which is nothing until there is more to
-- show than the view holds.
pickerBarW :: Float
pickerBarW = 10

rowHeight, codeHeight :: FontMetrics -> Float
rowHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 2
codeHeight fm = max 1 (fromIntegral (ceiling (fmLineHeight fm) :: Int))

-- | The finder over the window, as a modal: what is behind it keeps its place
-- and takes nothing, and the panel is the one thing the pointer can reach.
--
-- It is declared whether or not the finder is up, so that nothing else in the
-- frame moves when it comes and goes. Pass the finder and keep what comes
-- back: 'Nothing' is the finder put away, and the item is one that was picked,
-- which puts it away as well.
pickerOverlay :: Ui :> es => Maybe Picker -> Eff es (Maybe Picker, Maybe Item)
pickerOverlay mpk = do
  winW <- windowWidth
  winH <- windowHeight
  -- The panel takes the window but for a strip of it round the edge, enough
  -- to see that the editor is still there under it. The modal's own frame --
  -- its padding at the sides and foot, and its title bar over the top -- is
  -- taken off first, so that the strip is the same all the way round.
  --
  -- Whole pixels: the modal fits itself to the panel in whole pixels, and a
  -- panel a fraction of a pixel taller than that overflows it, which puts a
  -- scrollbar's lane down the modal's right that covers the panel's edge.
  let bodyW = whole (clamp 480 1600 (winW - 2 * pickerMargin - modalSides))
      bodyH = whole (clamp 260 1100 (winH - 2 * pickerMargin - modalChrome))
      whole v = fromIntegral (floor v :: Int)
  (closeResp, out) <-
    modal (isJust mpk) (maybe "" (srcTitle . pkSource) mpk) $
      case mpk of
        Nothing -> pure (Nothing, Nothing)
        Just pk -> pickerBody bodyW bodyH pk
  let (kept, chosen) = fromMaybe (Nothing, Nothing) out
      left = if respClicked closeResp then Nothing else kept
  -- Whatever put it away -- Escape, a pick, the panel's own button -- the
  -- thread that is gathering for it is told to stop.
  case (mpk, left) of
    (Just pk, Nothing) -> uiIO (closePicker pk)
    _ -> pure ()
  pure (left, chosen)

-- | How much of the window is left round the panel, and what the modal's frame
-- adds to the panel: its padding at either side, and its title bar, the rule
-- under it, the gap under that and its padding at the foot.
pickerMargin, modalSides, modalChrome :: Float
pickerMargin = 20
modalSides = 20
modalChrome = 61

-- | The panel: the prompt over the rows, with the preview beside them, and the
-- keys that work here along the foot.
pickerBody :: Ui :> es => Float -> Float -> Picker -> Eff es (Maybe Picker, Maybe Item)
pickerBody bodyW bodyH pk0 = do
  ctx <- askContext
  (fm, _) <- uiIO (ctxResolveFont ctx pickerFontSize WeightNormal FontStyleNormal FontMono)
  cellW <- uiIO (cellWidth fm)
  columnWith (tight . gap 0 . fixedW bodyW . fixedH bodyH) $ do
    query <- promptRow pk0
    pk1 <- uiIO (restock pk0 query)
    separator
    (pk3, chosen, closed) <- rowWith (grow . gap 0 . padAll 0) $ do
      (pk2, chosen, closed) <- rowsPane fm cellW bodyW pk1
      separator
      pk3 <- uiIO (ensurePreview pk2) >>= previewPane fm cellW
      pure (pk3, chosen, closed)
    separator
    keyHints
    pure (if closed || isJust chosen then Nothing else Just pk3, chosen)

-- | The keys that work in the finder, along its foot. Each key is in the
-- text's colour and what it does is muted beside it, so that the keys are
-- what the eye finds.
keyHints :: Ui :> es => Eff es ()
keyHints =
  rowWith (padLRTB 0 0 8 0 . tight . fillW . gap 16 . alignMid) $
    mapM_
      ( \(k, what) -> rowWith (tight . gap 5 . alignMid) $ do
          labelWith (tight . alignMid) k
          labelWith (tight . fontMuted . alignMid) what
      )
      [ ("Enter", "open")
      , ("\x2191 \x2193", "move")
      , ("Ctrl+D  Ctrl+U", "scroll the file")
      , ("Esc", "close")
      ]

--------------------------------------------------------------------------------
-- The prompt
--------------------------------------------------------------------------------

-- | The line along the top: what is being looked for, and how much of what
-- there is answers to it.
--
-- The field keeps the keyboard for as long as the finder is up, so that
-- everything typed goes to the query and the rows are left to the arrows. The
-- count is of the frame before this one: it is drawn above the rows and so
-- before they are worked out, and the application asks for another frame when
-- the answer has moved on.
promptRow :: Ui :> es => Picker -> Eff es Text
promptRow pk =
  rowWith (padXY 0 0 . tight . fillW . gap 8 . alignMid) $ do
    (resp, txt) <- textInput' (pkQuery pk)
    ctx <- askContext
    uiIO (takeFocus ctx (respId resp))
    labelWith (tight . fontMuted . alignMid) (counted pk)
    pure txt

-- | What answered, out of what there is: @48/1203@, with the gatherer's own
-- progress while it is still running.
counted :: Picker -> Text
counted pk =
  showT (hitCount pk) <> "/" <> showT (pkTaken pk) <> (if pkDone pk then "" else "\x2026")
  where
    showT :: Int -> Text
    showT = T.pack . show

--------------------------------------------------------------------------------
-- The rows
--------------------------------------------------------------------------------

-- | The rows that answer the query, in one widget that scrolls itself: a list
-- of a hundred thousand draws the twenty that are on screen and no others.
--
-- This is where the finder's keys are read, since it is the rows they move.
-- What comes back is the finder as the frame leaves it, an item that was
-- picked, and whether the finder was put away.
rowsPane :: Ui :> es => FontMetrics -> Float -> Float -> Picker -> Eff es (Picker, Maybe Item, Bool)
rowsPane fm cellW bodyW pk0 = do
  ctx <- askContext
  inp <- askInput
  wid <- nextId
  prev <- uiIO (getPrevRect ctx wid)
  let rect = fromMaybe (Rect 0 0 320 400) prev
      lineH = rowHeight fm
      viewRows = realToFrac (rectH rect / lineH) :: Double
      count = hitCount pk0
      mods = inputModifiers inp
      ctrl = modCtrl mods && not (modAlt mods)
      chord c = ctrl && T.any (== c) (inputChars inp)

      -- The keyboard. Enter takes what the keyboard is on and Escape puts the
      -- finder away; the rest is walking the rows, either with the arrows or
      -- with the chords a terminal's own finder walks them with.
      step k = case k of
        KeyUp -> -1
        KeyDown -> 1
        _ -> 0 :: Int
      moved =
        foldInputKeys (\acc k -> acc + step k) 0 (inputKeys inp)
          + (if chord 'n' then 1 else 0)
          - (if chord 'p' then 1 else 0)
      chosenByKey = inputKeysElem KeyEnter (inputKeys inp)
      closed = inputKeysElem KeyEscape (inputKeys inp)

      -- The pointer. A press takes the row under it, as a press on the file
      -- tree takes a file.
      mouse = inputMousePos inp
      inside = rectContains rect mouse
      localY = v2Y mouse - rectY rect
      overBar = inside && v2X mouse >= rectX rect + rectW rect - pickerBarW && fromIntegral count > viewRows
      pointed = floor (pkScroll pk0 + realToFrac (localY / lineH)) :: Int
      onRow = inside && not overBar && pointed >= 0 && pointed < count && isNothing grab
      pressed = onRow && inputMousePressed inp

      -- The scrollbar. A press on it takes hold of the thumb, and the rows
      -- follow the pointer until the button comes up, wherever it has gone.
      bar = rowsScroller (rectH rect) viewRows count
      grab
        | inputMousePressed inp && overBar = Just (thumbGrab bar (pkScroll pk0) localY)
        | inputMouseDown inp = pkGrab pk0
        | otherwise = Nothing

      cursor = clamp 0 (max 0 (count - 1)) (if pressed then pointed else pkCursor pk0 + moved)

      -- The wheel, three rows a notch, and then the row the keyboard is on
      -- kept in view.
      V2 _ wheelY = if inside then inputScroll inp else V2 0 0
      scrolled = maybe (pkScroll pk0 + realToFrac wheelY * 3) (\g -> thumbScroll bar g localY) grab
      followed = if moved /= 0 then followRow cursor viewRows scrolled else scrolled
      atRow = clamp 0 (max 0 (fromIntegral count - viewRows)) followed

      pk1 = pk0 {pkCursor = cursor, pkScroll = atRow, pkHovered = if onRow then pointed else -1, pkGrab = grab}
      chosen = if chosenByKey || pressed then currentItem pk1 else Nothing

  -- nano-ui runs a frame for a pointer that only moved when it came over
  -- another widget, and every row here is the one widget; while the pointer is
  -- over the rows the finder asks for its own frames, so the row under it
  -- keeps up. A gatherer that is still running wants them for the same reason:
  -- nothing else knows that more rows have arrived.
  when (inside || isJust grab || not (pkDone pk1)) (wakeAfter 0.03)

  let first = max 0 (floor atRow)
      last' = min (count - 1) (first + ceiling (rectH rect / lineH))
  shown <- uiIO (visibleRows pk1 first last')
  let widest = foldl' (\m (PickRow item _) -> max m (T.length (fileNameOf (itemText item)))) 0 shown
      pk2 = pk1 {pkNameCells = max (pkNameCells pk1) (min nameCap widest)}

  let scene =
        RowScene
          { rsRows = shown
          , rsFirst = first
          , rsCount = count
          , rsScroll = atRow
          , rsLineH = lineH
          , rsTextH = fmLineHeight fm
          , rsCellW = cellW
          , rsViewRows = viewRows
          , rsCursor = cursor
          , rsHovered = pkHovered pk1
          , rsEmpty = emptyNote pk1
          , rsNameCells = pkNameCells pk2
          , rsKey =
              contentHash
                [ hashText (pkQuery pk1)
                , pkTaken pk1
                , count
                , cursor
                , round (atRow * 64)
                , pkHovered pk1
                , fromEnum (pkDone pk1)
                , pkNameCells pk2
                , -- How many rows the view holds. The first frame has no
                  -- rect yet and guesses; the frame after it knows, and
                  -- draws the rows its guess left out.
                  first
                , last'
                ]
          }
  _ <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (fillH . fixedW (rowsWidth bodyW cellW)) defaultLayout
        , widgetDraw = \cdc r -> drawRows cdc scene r
        , widgetContent = rsKey scene
        , widgetCursor = Just (const UiCursorDefault)
        , widgetDamageSlop = 0
        }
  pure (pk2, chosen, closed)

-- | The rows' scrollbar, over a view so many rows high of so many rows.
rowsScroller :: Float -> Double -> Int -> Scroller
rowsScroller h viewRows count =
  let rows = fromIntegral count
   in Scroller h (viewRows / max 1 rows) (max 0 (rows - viewRows))

-- | How wide the column of rows is: enough for a name and a middling folder,
-- and never more than its share of the panel. The preview takes what is left,
-- which on a window of any size is most of it -- a path is scanned, and the
-- file it names is read.
rowsWidth :: Float -> Float -> Float
rowsWidth bodyW cellW = fromIntegral (round (min (bodyW * 0.42) (60 * cellW)) :: Int)

-- | The widest the column of names grows. A name longer than this pushes its
-- own folder along rather than every row's.
nameCap :: Int
nameCap = 36

-- | What stands in for the rows when there are none: whether nothing answered
-- the query, or there is nothing to answer it yet.
emptyNote :: Picker -> Text
emptyNote pk
  | hitCount pk > 0 = ""
  | pkTaken pk == 0 && not (pkDone pk) = "Looking\x2026"
  | T.null (pkQuery pk) = "Nothing here."
  | otherwise = "No match for " <> pkQuery pk

-- | The rows on screen, each with the characters of it the query matched.
-- Asking costs about as much as scoring the row again, so it is asked for the
-- twenty rows that are drawn and not for the hundred thousand that are not.
visibleRows :: Picker -> Int -> Int -> IO (SmallArray PickRow)
visibleRows pk first last' = do
  rows <- traverse oneRow [first .. last']
  pure (smallArrayFromList rows)
  where
    plain = T.null (matchQuery pk)
    oneRow i = case hitItem pk i of
      Nothing -> pure (PickRow (Item "" "" Nothing) U.empty)
      Just item -> do
        pos <-
          if plain
            then pure U.empty
            else Fuzzy.matchPositions (pkSlab pk) (pkPattern pk) (itemText item)
        pure (PickRow item pos)

-- | A row as it is drawn: what it is, and which of its characters answered
-- the query.
data PickRow = PickRow !Item !(U.Vector Int)

-- | Everything the rows' drawing reads.
data RowScene = RowScene
  { rsRows :: !(SmallArray PickRow)
  , rsFirst :: !Int
  , rsCount :: !Int
  , rsScroll :: !Double
  , rsLineH :: !Float
  , rsTextH :: !Float
  -- ^ The height of a line of the font the rows are set in, which is the
  -- editor's and not the one the drawing is handed.
  , rsCellW :: !Float
  , rsViewRows :: !Double
  , rsCursor :: !Int
  , rsHovered :: !Int
  , rsEmpty :: !Text
  , rsNameCells :: !Int
  , rsKey :: !Int
  }

-- | The draw ops of the rows on screen, and of the scrollbar beside them when
-- there is more than the view holds.
drawRows :: CustomDrawContext -> RowScene -> Rect -> SmallArray DrawOp
drawRows cdc sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect (tcPanel tc) : body)
  where
    theme = cdcTheme cdc
    tc = treeColors theme
    lineH = rsLineH sc
    cellW = rsCellW sc
    rows = rsRows sc
    shown = sizeofSmallArray rows
    yOff = realToFrac (fromIntegral (rsFirst sc) - rsScroll sc) * lineH
    rowY j = y + yOff + fromIntegral j * lineH
    textY ry = ry + (lineH - rsTextH sc) / 2
    lane = if fromIntegral (rsCount sc) > rsViewRows sc then pickerBarW else 0
    body
      | rsCount sc <= 0 =
          [ DrawTextStyled
              (x + pickerMark + pickerPad)
              (textY y)
              (TextFont pickerFontSize FontMono WeightNormal FontStyleNormal DecorationNone)
              (rsEmpty sc)
              (tcMuted tc)
          ]
      | otherwise = concatMap rowOps [0 .. shown - 1] ++ bar

    rowOps j =
      let PickRow item pos = indexSmallArray rows j
          i = rsFirst sc + j
          ry = rowY j
          picked = i == rsCursor sc
          pick = Rect (x + pickerMark + 2) (ry + 1) (max 0 (w - pickerMark - 4 - lane)) (lineH - 2)
          backdrop
            | picked = [FillRoundedRect pick radius (tcPicked tc)]
            | i == rsHovered sc = [FillRoundedRect pick radius (tcHover tc)]
            | otherwise = []
          -- The mark down the left of the row the keyboard is on is the
          -- caret's colour, which is the window's one way of saying where you
          -- are.
          mark = [FillRect (Rect x (ry + 1) pickerMark (lineH - 2)) (tcCurrent tc) | picked]
          ix = x + pickerMark + pickerPad
          tx = ix + pickerIcon
          cells = max 0 (floor ((w - (tx - x) - pickerPad - lane) / cellW))
          -- The name first, since it is what is being looked for, and the
          -- folder it is in beside it, in a column of its own, since that is
          -- how it is told from another file of the same name. A folder too
          -- long for what is left loses its front: the end of it is the part
          -- nearest the file.
          full = itemText item
          name = fileNameOf full
          base = T.length full - T.length name
          folder = if base > 0 then T.take (base - 1) full else ""
          folderCell = max (rsNameCells sc + 3) (T.length name + 3)
          (folderShown, dropped) = clipFront (cells - folderCell) folder
          matched k = U.elem k pos
          nameColor k = if matched (base + k) then themeYellow theme else tcName tc
          folderColor k = if matched (k + dropped) then themeYellow theme else tcMuted tc
          run cell0 txt colorOf =
            concat
              [ cellGlyphs (tx + fromIntegral (cell0 + c) * cellW) (textY ry) cellW (font WeightNormal) col seg
              | (c, seg, col) <- runsOf txt colorOf
              ]
       in backdrop
            ++ mark
            ++ fileIcon ix (ry + lineH / 2) (languageTint theme (itemPath item))
            ++ run 0 (T.take cells name) nameColor
            ++ run folderCell folderShown folderColor

    font weight = TextFont pickerFontSize FontMono weight FontStyleNormal DecorationNone
    radius = 3

    bar
      | lane <= 0 = []
      | otherwise =
          let (thumbTop, thumbH) = thumbSpan (rowsScroller h (rsViewRows sc) (rsCount sc)) (rsScroll sc)
           in [ FillRoundedRect
                  (Rect (x + w - pickerBarW + 2) (y + thumbTop + 2) (pickerBarW - 4) (thumbH - 4))
                  radius
                  (tcThumb tc)
              ]

-- | The name at the end of a path the rows show, which is always written with
-- forward slashes.
fileNameOf :: Text -> Text
fileNameOf = T.takeWhileEnd (/= '/')

-- | A text cut to a number of cells, losing its front rather than its end,
-- and how many characters went. What is left starts with an ellipsis, which
-- takes a cell of its own.
clipFront :: Int -> Text -> (Text, Int)
clipFront cells txt
  | cells <= 0 = ("", 0)
  | T.length txt <= cells = (txt, 0)
  | cells <= 1 = ("\x2026", T.length txt)
  | otherwise = ("\x2026" <> T.takeEnd (cells - 1) txt, T.length txt - (cells - 1) - 1)

-- | A text split into the longest runs of one colour, each with the cell it
-- starts at. The colour of a character is asked for by its index, so a run
-- ends wherever the answer changes.
runsOf :: Text -> (Int -> Color) -> [(Int, Text, Color)]
runsOf txt colorAt = go 0 (T.unpack txt)
  where
    go _ [] = []
    go !i (c : cs) =
      let col = colorAt i
          (run, rest) = span (\(k, _) -> sameColor (colorAt k) col) (zip [i + 1 ..] cs)
          seg = T.pack (c : map snd run)
       in (i, seg, col) : go (i + length run + 1) (map snd rest)
    sameColor a b = colorToWord32 a == colorToWord32 b

-- | The ops of a run of text a character to a cell, each placed on its own.
--
-- The rows are drawn this way rather than a run to an op because their runs
-- move: a run is one colour, and every key typed at the prompt changes which
-- characters are matched and so where the runs split. The toolkit puts a
-- run's first glyph on the pixel grid and the rest at whole pixels from it,
-- so a glyph lands where the run it is in started as well as where its own
-- cell is, and a glyph whose run started elsewhere this frame steps a pixel
-- sideways. Placed on its own, a glyph is where its cell is and nowhere else.
cellGlyphs :: Float -> Float -> Float -> TextFont -> Color -> Text -> [DrawOp]
cellGlyphs x0 y cellW font col txt =
  [ DrawTextStyled (x0 + fromIntegral i * cellW) y font (T.singleton c) col
  | (i, c) <- zip [0 :: Int ..] (T.unpack txt)
  , c /= ' '
  ]

-- | The ops of a run of text on the cell grid. A run of plain ASCII is one
-- op; anything else is placed a character at a time, since a glyph a fallback
-- font draws advances further than a cell and the rest of the run would walk
-- off its cells.
cellRun :: Float -> Float -> Float -> TextFont -> Color -> Text -> [DrawOp]
cellRun x0 y cellW font col txt
  | T.null txt || T.all (== ' ') txt = []
  | T.all simple txt = [DrawTextStyled x0 y font txt col]
  | otherwise =
      [ DrawTextStyled (x0 + fromIntegral i * cellW) y font (T.singleton c) col
      | (i, c) <- zip [0 :: Int ..] (T.unpack txt)
      , c /= ' '
      ]
  where
    simple c = c >= ' ' && c < '\x7F'

--------------------------------------------------------------------------------
-- The preview
--------------------------------------------------------------------------------

-- | The file beside the rows: a heading that says what it is, over the head of
-- it in the colours the editor would open it in.
previewPane :: Ui :> es => FontMetrics -> Float -> Picker -> Eff es Picker
previewPane fm cellW pk0 =
  columnWith (tight . gap 0 . grow . fillH) $ do
    previewHeading pk0
    previewBody fm cellW pk0

-- | Where the file is and what it is. The folder is muted and the name is not,
-- as they are in the rows; the length and the language are said the way the
-- status bar says them of the file that is open.
previewHeading :: Ui :> es => Picker -> Eff es ()
previewHeading pk =
  rowWith (padXY 10 6 . tight . fillW . gap 12 . alignMid) $ case currentItem pk of
    Nothing -> labelWith (tight . fontMuted . alignMid) " "
    Just item -> do
      let full = itemText item
          name = fileNameOf full
          folder = T.dropEnd (T.length name) full
          pv = pkPreview pk
          n = sizeofSmallArray (pvLines pv)
          lines'
            | pvMore pv = "More than " <> showT n <> " lines"
            | n == 1 = "1 line"
            | otherwise = showT n <> " lines"
      rowWith (tight . gap 0 . alignMid) $ do
        unless (T.null folder) $ labelWith (tight . fontMuted . alignMid) folder
        labelWith (tight . fontSemiBold . alignMid) name
      flex
      when (T.null (pvNote pv) && pvOf pv == Just (itemPath item)) $ do
        labelWith (tight . fontMuted . alignMid) lines'
        labelWith (tight . fontMuted . alignMid) (langName (languageFor (itemPath item)))
  where
    showT :: Int -> Text
    showT = T.pack . show

previewBody :: Ui :> es => FontMetrics -> Float -> Picker -> Eff es Picker
previewBody fm cellW pk0 = do
  ctx <- askContext
  inp <- askInput
  wid <- nextId
  prev <- uiIO (getPrevRect ctx wid)
  let rect = fromMaybe (Rect 0 0 400 400) prev
      pv = pkPreview pk0
      lineH = codeHeight fm
      viewLines = realToFrac (rectH rect / lineH) :: Double
      count = sizeofSmallArray (pvLines pv)
      mods = inputModifiers inp
      ctrl = modCtrl mods && not (modAlt mods)
      chord c = ctrl && T.any (== c) (inputChars inp)
      half = fromIntegral (max 1 (floor viewLines `div` 2 :: Int))
      -- The preview is read rather than walked, so it is the wheel and the
      -- chords a pager scrolls with that move it, and never the arrows: those
      -- are the rows'.
      V2 _ wheelY = if rectContains rect (inputMousePos inp) then inputScroll inp else V2 0 0
      asked = realToFrac wheelY * 3 + (if chord 'd' then half else 0) - (if chord 'u' then half else 0)
      -- A preview that has just been read opens where it is about: the top of
      -- a file, or the line a grep found.
      from = if pvScroll pv < 0 then maybe 0 (\ln -> fromIntegral ln - viewLines / 3) (pvHit pv) else pvScroll pv
      atLine = clamp 0 (max 0 (fromIntegral count - viewLines)) (from + asked)
      pk1 = pk0 {pkPreview = pv {pvScroll = atLine}}
      first = max 0 (floor atLine)
      last' = min (count - 1) (first + ceiling (rectH rect / lineH))
      digits = max 2 (length (show (max 1 count)))
      scene =
        CodeScene
          { csLines = pvLines pv
          , csFirst = first
          , csLast = last'
          , csCount = count
          , csScroll = atLine
          , csLineH = lineH
          , csTextH = fmLineHeight fm
          , csCellW = cellW
          , csGutterW = fromIntegral (digits + 2) * cellW
          , csHit = pvHit pv
          , csNote = pvNote pv
          , csKey = contentHash [pvVersion pv, round (atLine * 64), first, last']
          }
  _ <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (grow . fillH) defaultLayout
        , widgetDraw = \cdc r -> drawCode cdc scene r
        , widgetContent = csKey scene
        , widgetCursor = Just (const UiCursorDefault)
        , widgetDamageSlop = 0
        }
  pure pk1

-- | Everything the preview's drawing reads.
data CodeScene = CodeScene
  { csLines :: !(SmallArray PreviewLine)
  , csFirst :: !Int
  , csLast :: !Int
  , csCount :: !Int
  , csScroll :: !Double
  , csLineH :: !Float
  , csTextH :: !Float
  , csCellW :: !Float
  , csGutterW :: !Float
  , csHit :: !(Maybe Int)
  , csNote :: !Text
  , csKey :: !Int
  }

-- | The draw ops of the preview: the numbers down its left, and the lines of
-- the file in the colours the editor sets them in.
drawCode :: CustomDrawContext -> CodeScene -> Rect -> SmallArray DrawOp
drawCode cdc sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect colBackground : body)
  where
    theme = cdcTheme cdc
    lineH = csLineH sc
    cellW = csCellW sc
    gutterW = csGutterW sc
    yOff = realToFrac (fromIntegral (csFirst sc) - csScroll sc) * lineH
    lineY ln = y + yOff + fromIntegral (ln - csFirst sc) * lineH
    textY ry = ry + (lineH - csTextH sc) / 2
    font kind = TextFont pickerFontSize FontMono (tokenWeight kind) FontStyleNormal DecorationNone
    textX = x + gutterW
    cells = max 0 (floor ((w - gutterW - pickerPad) / cellW))
    body
      | not (T.null (csNote sc)) =
          [ DrawTextStyled (x + pickerPad) (textY y) (font TokPlain) (csNote sc) (themeMuted theme)
          ]
      | otherwise = FillRect (Rect x y gutterW h) colGutter : concatMap lineOps [csFirst sc .. csLast sc]

    lineOps ln =
      let PreviewLine txt spans = indexSmallArray (csLines sc) ln
          ry = lineY ln
          hit = csHit sc == Just ln
          band = [FillRect (Rect x ry w lineH) colCurrentLine | hit]
          num = T.pack (show (ln + 1))
          number =
            DrawTextStyled
              (x + gutterW - cellW - fromIntegral (T.length num) * cellW)
              (textY ry)
              (font TokPlain)
              num
              (if hit then colGutterActive else colGutterText)
       in band ++ [number] ++ spanOps (textY ry) 0 (T.take cells txt) (takeSpans cells spans)

    spanOps _ _ _ [] = []
    spanOps ry !cell txt (Span n kind : rest) =
      cellRun (textX + fromIntegral cell * cellW) ry cellW (font kind) (tokenColor kind) (T.take n txt)
        ++ spanOps ry (cell + n) (T.drop n txt) rest

-- | The spans of the first so many characters of a line.
takeSpans :: Int -> [Span] -> [Span]
takeSpans _ [] = []
takeSpans n (Span len kind : rest)
  | n <= 0 = []
  | len >= n = [Span n kind]
  | otherwise = Span len kind : takeSpans (n - len) rest
