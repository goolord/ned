-- | The fuzzy finder: a prompt, the rows that answer it, and a preview of the
-- one the keyboard is on.
--
-- Ctrl+P puts it up over the window. What it is looking through is a 'Source',
-- which gathers its rows on a thread of its own and feeds them over as it
-- finds them, so a picker over a hundred thousand files is usable from its
-- first frame and the window never waits on a disk. The editor has two:
-- 'fileSource', every file under the tree's root, in "Ned.Picker.File", and
-- 'grepSource', every line under it that answers the query, in
-- "Ned.Picker.Grep". The second answers the query itself rather than leaving
-- it to the matcher, which is what 'srcLive' says, and it is asked again only
-- once typing has paused: the prompt is a search input, which says when.
--
-- Scoring is fzf's own, through "Ned.Fuzzy": one call scores the whole
-- list, and the rows on screen ask again for which of their characters the
-- query matched, which is what the finder colours them by.
--
-- The finder is one module, state and frame and drawing together, so
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
  , pickerHovered
  , pickerTop

    -- * What it looks through
  , Item (..)
  , Sink
  , Source (..)
  , GatherFailed (..)
  , fileSource
  , grepSource
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (unless, void, when)
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (groupBy, mapAccumL, scanl')
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Clock (getMonotonicTime)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Effectful (Eff, type (:>))
import NanoUI
import Ned.Editor (cellWidth, defaultFontSize)
import qualified Ned.Fuzzy as Fuzzy
import Ned.Highlight
import Ned.Picker.File (fileSource)
import Ned.Picker.Grep (grepSource)
import Ned.Picker.Source
import Ned.Text (cellsAt, clamp)
import Ned.Theme
import Ned.Widget
import System.FilePath (takeFileName)
import System.IO (IOMode (ReadMode), withBinaryFile)

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
  , gatFailed :: !(Maybe Text)
  -- ^ Why the gatherer stopped short, when it did.
  }

-- | The finder, between frames.
data Picker = Picker
  { pkSource :: !Source
  , pkRoot :: !FilePath
  , pkTyped :: !Text
  -- ^ What is in the prompt. A live source is asked only once typing has
  -- paused, so for a while this is ahead of the query.
  , pkQuery :: !Text
  -- ^ What the rows answer.
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
  , pkFailed :: !(Maybe Text)
  , pkStale :: !(Maybe Double)
  -- ^ While a new query is being gathered for, the rows on screen are the
  -- last query's, kept until the new one has something to put in their place
  -- or this moment has passed. Emptying them at once would blank the list for
  -- as long as ripgrep takes to answer, and a list that blinks at every word
  -- is harder to read than one that is briefly behind.
  , pkItems :: !(V.Vector Item)
  , pkCands :: !Fuzzy.Candidates
  -- ^ The same rows, laid out for the matcher to scan in one call.
  , pkHits :: !Fuzzy.Matches
  -- ^ What matched, best first, as indices into 'pkItems'.
  , pkCursor :: !Int
  -- ^ Which hit the keyboard is on.
  , pkScroll :: !Double
  -- ^ The hit at the top of the list, as the rows' scroller last had it; its
  -- fraction is how far it is scrolled out of view.
  , pkHovered :: !Int
  , pkToTop :: !Bool
  -- ^ Whether the rows go back to the top on the next frame: a new query, or
  -- a finder just put up, whose scroller may still be where the last one
  -- was left.
  , pkNameCells :: !Int
  -- ^ How many cells the rows' column of names is wide: the longest name
  -- the rows have shown since the query last changed. It only grows, so
  -- that the folders beside the names stay put while the list is scrolled.
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
  , pvFirst :: !Int
  -- ^ The line of the file the first of 'pvLines' is. A preview of a hit far
  -- down a file is the lines round it, and not the head of the file.
  , pvTotal :: !Int
  -- ^ How many lines the file has, as far as it was read.
  , pvWhole :: !(V.Vector Text)
  -- ^ Every line of the file that was read, kept so that another hit in the
  -- same file is shown from them rather than by reading the file again.
  , pvMore :: !Bool
  -- ^ Whether the file goes on past what was read.
  , pvScroll :: !Double
  -- ^ Below zero for a preview that has not been placed yet: it opens on
  -- what it is about, and after that it is where the reader scrolled it.
  , pvVersion :: !Int
  -- ^ Bumped whenever the lines change, for the drawing's content key.
  }

-- | A line of the preview, lexed when the file was read rather than on every
-- frame, with the cells on it a grep found, from a cell to the one past it.
data PreviewLine = PreviewLine !Text ![Span] ![(Int, Int)]

emptyPreview :: Preview
emptyPreview =
  Preview
    { pvOf = Nothing
    , pvLines = emptySmallArray
    , pvNote = ""
    , pvHit = Nothing
    , pvFirst = 0
    , pvTotal = 0
    , pvWhole = V.empty
    , pvMore = False
    , pvScroll = 0
    , pvVersion = 0
    }

-- | Put a finder up over a root, and set its gatherer going.
openPicker :: Source -> FilePath -> IO Picker
openPicker source root = do
  slab <- Fuzzy.newSlab
  pattern_ <- Fuzzy.compile (Fuzzy.defaultQuery "")
  cell <- newIORef (Gathered 0 [] 0 False Nothing)
  gather Nothing $
    Picker
      { pkSource = source
      , pkRoot = root
      , pkTyped = ""
      , pkQuery = ""
      , pkSlab = slab
      , pkPattern = pattern_
      , pkCell = cell
      , pkGen = 0
      , pkTaken = 0
      , pkDone = False
      , pkFailed = Nothing
      , pkStale = Nothing
      , pkItems = V.empty
      , pkCands = Fuzzy.candidates V.empty
      , pkHits = Fuzzy.noMatches
      , pkCursor = 0
      , pkScroll = 0
      , pkHovered = -1
      , pkToTop = True
      , pkNameCells = 0
      , pkPreview = emptyPreview
      }

-- | Put the finder away, and with it the thread that is gathering for it: the
-- generation moves on, so the next batch it offers is refused and it stops.
closePicker :: Picker -> IO ()
closePicker pk = writeIORef (pkCell pk) (Gathered (pkGen pk + 1) [] 0 True Nothing)

-- | Start gathering for the query the picker holds. What an earlier gather
-- found stays on screen, marked stale, until the moment given if there is one
-- or until 'harvest' has something to show instead; with none it goes at once.
gather :: Maybe Double -> Picker -> IO Picker
gather keepUntil pk = do
  let gen = pkGen pk + 1
      cell = pkCell pk
  writeIORef cell (Gathered gen [] 0 False Nothing)
  _ <-
    forkIO $ do
      ran <- try (srcGather (pkSource pk) (pkRoot pk) (pkQuery pk) (batchSink cell gen))
      let failed = either (Just . failure) (const Nothing) ran
      atomicModifyIORef' cell $ \g -> (if gatGen g == gen then g {gatDone = True, gatFailed = failed} else g, ())
  pure
    pk
      { pkGen = gen
      , pkTaken = 0
      , pkDone = False
      , pkFailed = Nothing
      , pkStale = keepUntil
      }

-- | What a gatherer that threw says in place of the rows.
failure :: SomeException -> Text
failure e = case fromException e of
  Just (GatherFailed msg) -> msg
  Nothing -> T.pack (displayException e)

batchSink :: IORef Gathered -> Int -> Sink
batchSink cell gen batch = atomicModifyIORef' cell take'
  where
    take' g
      | gatGen g /= gen = (g, False)
      | null batch = (g, True)
      | otherwise = (g {gatBatches = batch : gatBatches g, gatCount = gatCount g + length batch}, True)

-- | A number that changes when anything the finder shows does, which is what
-- tells the application that the frame it drew is no longer what the finder
-- says and another is wanted. A gatherer that found more rows between frames
-- changes it without anybody having touched a key.
pickerSig :: Maybe Picker -> Int
pickerSig Nothing = 0
pickerSig (Just pk) =
  contentKeyOf
    [ keyPart (pkQuery pk)
    , keyPart (pkTyped pk == pkQuery pk)
    , keyPart (pkTaken pk)
    , keyPart (hitCount pk)
    , keyPart (pkCursor pk)
    , keyPart (pkScroll pk)
    , keyPart (pkHovered pk)
    , keyPart (pkDone pk)
    , keyPart (isJust (pkStale pk))
    , keyPart (pvVersion (pkPreview pk))
    , keyPart (pvScroll (pkPreview pk))
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

-- | The row the pointer is over, counted from the top of the rows, or -1
-- when it is over none of them.
pickerHovered :: Picker -> Int
pickerHovered = pkHovered

-- | Take in whatever the gatherer has found since the last frame, and answer
-- the query against it.
--
-- The query is the matcher's when the source gathered everything at once, and
-- the gatherer's when the source answers it itself. The matcher is quick
-- enough to be asked on every keystroke, and takes the prompt as it stands; a
-- live source is asked only when the prompt says typing has paused, and then
-- starts again rather than filtering what the old query found. What the old
-- query found stays up until the new one has rows, which is when the keyboard
-- goes back to the top.
restock :: Picker -> Text -> Bool -> IO Picker
restock pk0 typed settled
  | live = do
      now <- getMonotonicTime
      pk1 <-
        if settled && typed /= pkQuery pk0
          then gather (Just (now + staleFor)) pk0 {pkTyped = typed, pkQuery = typed}
          else pure pk0 {pkTyped = typed}
      pk <- harvest now pk1
      -- The stale rows were stood down because the new query has rows in
      -- their place, which is a new list: the keyboard goes back to the top.
      -- Rows that merely arrived from the gatherer leave it where it was.
      let staleCleared = isJust (pkStale pk1) && isNothing (pkStale pk)
      if staleCleared
        then rematch True pk
        else if pkTaken pk /= pkTaken pk1 then rematch False pk else pure pk
  | otherwise = do
      pk <- harvest 0 pk0 {pkTyped = typed, pkQuery = typed}
      if changed || pkTaken pk /= pkTaken pk0 then rematch changed pk else pure pk
  where
    live = srcLive (pkSource pk0)
    changed = typed /= pkQuery pk0

-- | How long the last query's rows are kept up while a new one finds nothing,
-- in seconds. A search that has not found anything by then may not, and rows
-- that answer another query are not left standing in for none.
staleFor :: Double
staleFor = 1

-- | Fold the batches the gatherer has left into the rows the matcher scans.
-- Stale rows are kept until the gatherer has handed some over, or has
-- finished, or has run out of the time it was given.
harvest :: Double -> Picker -> IO Picker
harvest now pk = pure . taken =<< readIORef (pkCell pk)
  where
    taken g
      -- Another gather is running; what this one left is not ours.
      | gatGen g /= pkGen pk = pk
      | keepStale = pk
      | gatCount g == pkTaken pk = settled
      | otherwise =
          settled
            { pkItems = items
            , pkCands = Fuzzy.candidates (V.map itemText items)
            , pkTaken = gatCount g
            }
      where
        -- While a new query is being gathered for, the rows on screen are
        -- kept up until it has something to put in their place.
        keepStale = case pkStale pk of
          Just until_ -> gatCount g == 0 && not (gatDone g) && now < until_
          Nothing -> False
        settled =
          pk {pkDone = gatDone g, pkFailed = gatFailed g, pkStale = Nothing}
        items = V.fromList (concat (reverse (gatBatches g)))

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
      , pkToTop = fresh || pkToTop pk
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
-- in a moment is not worth reading twice. A hit is further in than the head
-- of a file, as far as a grep found it, so a preview of one reads further.
previewBytes, hitBytes, previewLines :: Int
previewBytes = 128 * 1024
hitBytes = 4 * 1024 * 1024
previewLines = 600

-- | Read the file the keyboard is on, if it is not the one already read. A
-- grep hit in the file that is up already is the lines round it, from what
-- was read of the file for the last one.
ensurePreview :: Picker -> IO Picker
ensurePreview pk = case currentItem pk of
  Nothing
    | isNothing (pvOf pv0) -> pure pk
    | otherwise -> pure (shown emptyPreview)
  Just item
    | pvOf pv0 == Just (itemPath item) && pvHit pv0 == itemLine item -> pure pk
    | pvOf pv0 == Just (itemPath item) && not (V.null (pvWhole pv0)) ->
        pure (shown (windowPreview item (fileRanges pk (itemPath item)) (pvMore pv0) (pvWhole pv0)))
    | otherwise -> shown <$> readPreview item (fileRanges pk (itemPath item))
  where
    pv0 = pkPreview pk
    shown pv = pk {pkPreview = pv {pvVersion = pvVersion pv0 + 1}}

-- | Where a grep found what it was looking for in a file, line by line: every
-- hit in it that has been gathered, and not only the one the keyboard is on,
-- as Ctrl+F marks every match in view. A file's hits are handed over together
-- once ripgrep has finished with it, so they are all here by the time it can
-- be previewed.
fileRanges :: Picker -> FilePath -> IM.IntMap [(Int, Int)]
fileRanges pk path
  | not (srcLive (pkSource pk)) = IM.empty
  | otherwise =
      IM.fromListWith
        (<>)
        [ (ln, itemRanges it)
        | it <- V.toList (pkItems pk)
        , Just ln <- [itemLine it]
        , not (null (itemRanges it))
        , itemPath it == path
        ]

-- | The head of a file, or the lines round a hit in it. Only the first
-- 'previewBytes' are read: what is past that is past what anybody previews,
-- and reading it would put a frame on hold for a file the reader may be
-- arrowing straight past.
readPreview :: Item -> IM.IntMap [(Int, Int)] -> IO Preview
readPreview item found = do
  let limit = if isJust (itemLine item) then hitBytes else previewBytes
  raw <- try (withBinaryFile (itemPath item) ReadMode (\h -> BS.hGet h limit))
  pure $ case raw of
    Left (_ :: SomeException) -> note "This file cannot be read."
    Right got
      | BS.null bytes -> note "This file is empty."
      | BS.elem 0 bytes -> note "This is not a text file."
      | otherwise -> windowPreview item found cut (V.fromList (dropLast cut (T.lines text)))
      where
        -- The mark a file may start with is not a character of its first
        -- line, to the editor or to ripgrep, which counts a hit on that line
        -- from after it.
        bytes = fromMaybe got (BS.stripPrefix "\xEF\xBB\xBF" got)
        text = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') bytes
        -- A file longer than the read ends mid-line, and half a line shown
        -- whole is a line that says something the file does not.
        cut = BS.length got >= limit
  where
    note msg = (previewOf item) {pvNote = msg}
    dropLast False xs = xs
    dropLast True xs = if null xs then xs else init xs

-- | A preview of an item, before anything is read of it. One nobody has
-- scrolled yet opens where the file is about, which the drawing works out
-- once it knows how tall the pane is.
previewOf :: Item -> Preview
previewOf item = emptyPreview {pvOf = Just (itemPath item), pvHit = itemLine item, pvScroll = -1}

-- | What the preview keeps of the lines of a file that were read: the head of
-- it, or the lines round a hit, lexed for colour, with what a grep found on
-- them marked. A hit past what was read is said to be, rather than the lines
-- that were read shown in its place.
--
-- The lines round a hit are lexed from the first of them, not from the top of
-- the file, so a hit inside a long comment may be coloured as code.
windowPreview :: Item -> IM.IntMap [(Int, Int)] -> Bool -> V.Vector Text -> Preview
windowPreview item found cut whole
  | Just ln <- itemLine item
  , ln >= total =
      kept
        { pvNote =
            if cut
              then "This line is further into the file than the preview reads."
              else "This line is past the end of the file, which has changed since it was searched."
        }
  | otherwise =
      kept
        { pvLines = smallArrayFromList (lexAll (languageFor (itemPath item)) [(l, IM.findWithDefault [] ln found) | (ln, l) <- zip [from ..] (V.toList ls)])
        , pvFirst = from
        }
  where
    kept = (previewOf item) {pvWhole = whole, pvTotal = total, pvMore = cut}
    total = V.length whole
    -- The hit in the middle of what is kept, unless the file ends first.
    from = maybe 0 (\ln -> max 0 (min (total - previewLines) (ln - previewLines `div` 2))) (itemLine item)
    ls = V.slice from (min previewLines (total - from)) whole

-- | The lines with what the lexer made of each, the state carrying from one
-- to the next as it does in the editor. Tabs are laid out before lexing, so
-- that a span's length is the cells it covers; what a grep found on a line is
-- counted in characters, and is laid out alongside.
lexAll :: Lang -> [(Text, [(Int, Int)])] -> [PreviewLine]
lexAll lang = go LexNormal
  where
    go _ [] = []
    go st ((l, ranges) : rest) =
      let (t, starts) = laidOut l
          (spans, st') = lexLine lang st t
          place o = starts U.! clamp 0 (T.length l) o
       in PreviewLine t spans [(place a, place b) | (a, b) <- ranges] : go st' rest

-- | What the preview makes of a line: the line with its tabs laid out as the
-- spaces they stand for, and where each of its characters starts in that
-- text, one past the last of them for the end of the line. A tab takes as
-- many places as it has spaces, and any other character one, so a range of
-- characters of the line is a pair of places in the text laid out.
laidOut :: Text -> (Text, U.Vector Int)
laidOut line = (expanded, starts)
  where
    places = snd (mapAccumL step 0 (T.unpack line))
    -- How many cells a character draws from where the walk has come to, and
    -- how many places it takes in the text laid out: a tab runs to the next
    -- tab stop and takes the spaces it ran over.
    step !cell c
      | c == '\t' = (cell + n, n)
      | otherwise = (cell + n, 1)
      where
        n = cellsAt cell c
    expanded
      | T.any (== '\t') line = T.pack (concat (zipWith render (T.unpack line) places))
      | otherwise = line
    render c n = if c == '\t' then replicate n ' ' else [c]
    -- The places characters start at, from how many each of them takes.
    starts = U.fromListN (T.length line + 1) (scanl' (+) 0 places)

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
  -- its title bar and padding -- is inside the panel, and the body fills
  -- what the frame leaves.
  --
  -- Whole pixels, so that the panel's edges land on the pixel grid.
  let panelW = whole (clamp 500 1620 (winW - 2 * pickerMargin))
      panelH = whole (clamp 320 1160 (winH - 2 * pickerMargin))
      whole v = fromIntegral (floor v :: Int)
  (closeResp, out) <-
    modalWith (fixedWH panelW panelH) (isJust mpk) (maybe "" (srcTitle . pkSource) mpk) $
      case mpk of
        Nothing -> pure (Nothing, Nothing)
        Just pk -> pickerBody pk
  let (kept, chosen) = fromMaybe (Nothing, Nothing) out
      left = if respClicked closeResp then Nothing else kept
  -- Whatever put it away -- Escape, a pick, the panel's own button -- the
  -- thread that is gathering for it is told to stop.
  case (mpk, left) of
    (Just pk, Nothing) -> uiIO (closePicker pk)
    _ -> pure ()
  pure (left, chosen)

-- | How much of the window is left round the panel.
pickerMargin :: Float
pickerMargin = 20

-- | The panel: the prompt over the rows, with the preview beside them, and the
-- keys that work here along the foot.
pickerBody :: Ui :> es => Picker -> Eff es (Maybe Picker, Maybe Item)
pickerBody pk0 = do
  fm <- resolveFontUi pickerFontSize WeightNormal FontStyleNormal FontMono
  cellW <- cellWidth (lineWidthUi fm)
  columnWith (tight . gap 0 . fillW . fillH) $ do
    (typed, settled) <- promptRow pk0
    pk1 <- uiIO (restock pk0 typed settled)
    -- The rule under the prompt spans the body, so where it was laid out
    -- last frame says how wide the body is, which the column of rows takes
    -- its share of. A share of the row it sits in would be squeezed by a
    -- preview of long lines beside it.
    ruleId <- currentId
    separator
    bodyW <- maybe 900 rectW <$> lastRect ruleId
    (pk3, chosen, closed) <- rowWith (grow . gap 0 . padAll 0) $ do
      (pk2, chosen, closed) <- rowsPane fm cellW (rowsWidth bodyW cellW) pk1
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
--
-- It is a search input, which says as well as what is typed whether typing
-- has paused: that is when a live source is asked again, so that a grep is run
-- for the word and not for every letter of it on the way. Enter says the
-- same, since it is asking for what was typed: the rows from before the
-- pause are for something else, and Enter does not open one of them.
promptRow :: Ui :> es => Picker -> Eff es (Text, Bool)
promptRow pk =
  rowWith (padXY 0 0 . tight . fillW . gap 8 . alignMid) $ do
    (resp, txt) <-
      searchInputConfigured'
        defaultSearchInputConfig {sicPlaceholder = srcPrompt (pkSource pk), sicDebounceMs = liveDebounceMs}
        (pkTyped pk)
    holdFocus (respId resp)
    labelWith (tight . fontMuted . alignMid) (counted pk)
    pure (txt, respChanged resp || respSubmitted resp)

-- | How long typing has to pause before a live source is asked, in
-- milliseconds.
liveDebounceMs :: Float
liveDebounceMs = 200

-- | What answered, out of what there is: @48/1203@, with the gatherer's own
-- progress while it is still running. A live source's rows all answer, so
-- what there is is not said.
counted :: Picker -> Text
counted pk
  | srcLive (pkSource pk) = showT (hitCount pk) <> pending
  | otherwise = showT (hitCount pk) <> "/" <> showT (pkTaken pk) <> pending
  where
    pending = if pkDone pk && pkTyped pk == pkQuery pk then "" else "\x2026"
    showT :: Int -> Text
    showT = T.pack . show

--------------------------------------------------------------------------------
-- The rows
--------------------------------------------------------------------------------

-- | The rows that answer the query, in a scroller that builds only the rows
-- in its view: a list of a hundred thousand draws the twenty that are on
-- screen and no others. The scroller is nano-ui's, and so are the wheel, the
-- scrollbar and its thumb; the rows in view are one custom widget between
-- two spacers that stand for the rows above and below them, since each
-- character a query matched is coloured on its own.
--
-- This is where the finder's keys are read, since it is the rows they move.
-- What comes back is the finder as the frame leaves it, an item that was
-- picked, and whether the finder was put away.
rowsPane :: Ui :> es => FontMetrics -> Float -> Float -> Picker -> Eff es (Picker, Maybe Item, Bool)
rowsPane fm cellW rowsW pk0 = do
  inp <- askInput
  sid <- currentId
  let lineH = rowHeight fm
      count = hitCount pk0
      mods = inputModifiers inp
      ctrl = modCtrl mods && not (modAlt mods)
      chord c = ctrl && T.any (== c) (inputChars inp)

      -- The keyboard. Enter takes what the keyboard is on; the rest is
      -- walking the rows, either with the arrows or with the chords a
      -- terminal's own finder walks them with.
      step k = case k of
        KeyUp -> -1
        KeyDown -> 1
        _ -> 0 :: Int
      moved =
        foldInputKeys (\acc k -> acc + step k) 0 (inputKeys inp)
          + (if chord 'n' then 1 else 0)
          - (if chord 'p' then 1 else 0)
      chosenByKey = inputKeysElem KeyEnter (inputKeys inp)
  -- Escape puts the finder away, unless it is the Escape that closes the
  -- prompt's own right-click menu.
  closed <- takeEscape

  -- The pointer. A press takes the row under it, as a press on the file tree
  -- takes a file. The rows are where the scroller's view last put them.
  metrics0 <- getScrollMetricsUi sid
  let viewport = maybe (Rect 0 0 320 400) scrollViewport metrics0
      offset0 = maybe 0 (v2Y . scrollOffset) metrics0
      mouse = inputMousePos inp
      pointed = floor ((v2Y mouse - rectY viewport + offset0) / lineH) :: Int
      onRow = rectContains viewport mouse && pointed >= 0 && pointed < count
      pressed = onRow && inputMousePressed inp
      cursor = clamp 0 (max 0 (count - 1)) (if pressed then pointed else pkCursor pk0 + moved)

  -- Three rows a notch, a new query back at the top, and the row the keyboard
  -- moved to kept in view.
  setScrollStepUi sid (3 * lineH)
  when (pkToTop pk0) (scrollToUi sid (V2 0 0) ScrollInstant)
  when (moved /= 0) (scrollRectIntoViewUi sid (Rect 0 (fromIntegral cursor * lineH) 1 lineH) ScrollNearest ScrollInstant)
  offset <- maybe offset0 (v2Y . scrollOffset) <$> getScrollMetricsUi sid

  let hovered = if onRow then pointed else -1
      first = clamp 0 (max 0 (count - 1)) (floor (offset / lineH))
      last' = min (count - 1) (first + ceiling (rectH viewport / lineH))
      pk1 = pk0 {pkCursor = cursor, pkScroll = realToFrac (offset / lineH), pkHovered = hovered, pkToTop = False}
      -- Stale rows answer the last query and not the one in the prompt, so
      -- nothing is opened from them.
      chosen = if (chosenByKey || pressed) && isNothing (pkStale pk0) then currentItem pk1 else Nothing

  -- A gatherer that is still running asks for frames of its own: nothing
  -- else knows that more rows have arrived.
  when (not (pkDone pk1)) (wakeAfter 0.03)

  shown <- uiIO (visibleRows pk1 first last')
  let widest = foldl' (\m (PickRow item _) -> max m (T.length (rowLead item))) 0 shown
      pk2 = pk1 {pkNameCells = max (pkNameCells pk1) (min nameCap widest)}
      -- How wide the rows may draw, which is the viewport: the scroller keeps
      -- its scrollbar's lane out of that, so a row held to it cannot run
      -- under the bar. Nothing has published a viewport before the scroller's
      -- first frame, so until then nothing is held back.
      roomW = maybe (1 / 0) (rectW . scrollViewport) metrics0
      scene =
        RowScene
          { rsRows = shown
          , rsFirst = first
          , rsLineH = lineH
          , rsTextH = fmLineHeight fm
          , rsCellW = cellW
          , rsCursor = cursor
          , rsHovered = hovered
          , rsNameCells = pkNameCells pk2
          , rsMaxW = roomW
          , rsKey =
              contentKeyOf
                [ keyPart (pkQuery pk1)
                , keyPart (pkTaken pk1)
                , keyPart count
                , keyPart cursor
                , keyPart hovered
                , keyPart (pkNameCells pk2)
                , keyPart first
                , keyPart last'
                , keyPart roomW
                ]
          }
      above = fromIntegral first * lineH
      inView = fromIntegral (max 0 (last' - first + 1)) * lineH
      below = fromIntegral (max 0 (count - last' - 1)) * lineH
  _ <-
    scrollArea (tight . gap 0 . fillH . fixedW rowsW) $
      if count <= 0
        then scope (emptyRows pk1)
        else scope $ do
          spacer Fit (Fixed above)
          _ <-
            customWidget
              defaultCustomWidgetSpec
                { widgetLayout = (fillW . fixedH inView) defaultLayout
                , widgetDraw = \cdc r -> drawRows cdc scene r
                , widgetContent = rsKey scene
                , widgetCursor = Just (const UiCursorDefault)
                , widgetDamageSlop = 0
                , -- Every row in view is this one widget, so the row under a
                  -- moving pointer keeps up only with a frame for every move
                  -- over it.
                  widgetTrackPointer = True
                }
          spacer Fit (Fixed below)
  pure (pk2, chosen, closed)

-- | What stands in for the rows when there are none, as text that wraps: a
-- gatherer's error can be longer than the column is wide.
emptyRows :: Ui :> es => Picker -> Eff es ()
emptyRows pk =
  columnWith (padXY (pickerMark + pickerPad) 6 . tight . fillW) $
    void (richTextWith (fillW . fontMuted) [inlineText (emptyNote pk)])

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
-- the query, or there is nothing to answer it yet, or the gatherer could not
-- look.
emptyNote :: Picker -> Text
emptyNote pk
  | hitCount pk > 0 = ""
  | Just msg <- pkFailed pk = msg
  | srcLive (pkSource pk) && T.null (T.strip (pkQuery pk)) = "Type to search."
  | pkTaken pk == 0 && not (pkDone pk) = "Looking\x2026"
  | T.null (pkQuery pk) = "Nothing here."
  | otherwise = "No match for " <> pkQuery pk

-- | The rows on screen, each with the characters of it the query matched.
-- Asking costs about as much as scoring the row again, so it is asked for the
-- twenty rows that are drawn and not for the hundred thousand that are not.
-- A live source has said already, and is not asked.
visibleRows :: Picker -> Int -> Int -> IO (SmallArray PickRow)
visibleRows pk first last' = do
  rows <- traverse oneRow [first .. last']
  pure (smallArrayFromList rows)
  where
    plain = T.null (matchQuery pk)
    oneRow i = case hitItem pk i of
      Nothing -> pure (PickRow (Item "" "" Nothing U.empty []) U.empty)
      Just item -> do
        pos <-
          if plain
            then pure (itemMarks item)
            else Fuzzy.matchPositions (pkSlab pk) (pkPattern pk) (itemText item)
        pure (PickRow item pos)

-- | A row as it is drawn: what it is, and which of its characters answered
-- the query.
data PickRow = PickRow !Item !(U.Vector Int)

-- | Everything the rows' drawing reads.
data RowScene = RowScene
  { rsRows :: !(SmallArray PickRow)
  , rsFirst :: !Int
  -- ^ The hit the first of the rows is.
  , rsLineH :: !Float
  , rsTextH :: !Float
  -- ^ The height of a line of the font the rows are set in, which is the
  -- editor's and not the one the drawing is handed.
  , rsCellW :: !Float
  , rsCursor :: !Int
  , rsHovered :: !Int
  , rsNameCells :: !Int
  , rsMaxW :: !Float
  -- ^ How wide the rows may draw: no more than the widget they sit in, and no
  -- more than the scroller's viewport, which leaves the scrollbar's lane out
  -- -- so a row's button can never be wider than the column it is in, nor
  -- reach the bar beside it.
  , rsKey :: !Int
  }

-- | The draw ops of the rows in view, the first of them at the top of the
-- widget.
drawRows :: CustomDrawContext -> RowScene -> Rect -> SmallArray DrawOp
drawRows cdc sc rect@(Rect x y w _) =
  smallArrayFromList (FillRect rect (tcPanel tc) : concatMap rowOps [0 .. shown - 1])
  where
    theme = cdcTheme cdc
    tc = treeColors theme
    lineH = rsLineH sc
    cellW = rsCellW sc
    rows = rsRows sc
    shown = sizeofSmallArray rows
    -- A row's button and its text answer to the viewport rather than to the
    -- widget: the scrollbar's lane is out of the viewport, so neither can be
    -- wider than the column the rows sit in, whatever the rows were laid out
    -- as. The panel behind them still fills the widget, or its edge would be
    -- a seam down the lane.
    roomW = min w (rsMaxW sc)
    rowY j = y + fromIntegral j * lineH
    textY ry = ry + (lineH - rsTextH sc) / 2

    rowOps j =
      let PickRow item pos = indexSmallArray rows j
          i = rsFirst sc + j
          ry = rowY j
          picked = i == rsCursor sc
          pick = Rect (x + pickerMark + 2) (ry + 1) (max 0 (roomW - pickerMark - 4)) (lineH - 2)
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
          cells = max 0 (floor ((roomW - (tx - x) - pickerPad) / cellW))
          -- The name first, since it is what is being looked for, and the
          -- folder it is in beside it, in a column of its own, since that is
          -- how it is told from another file of the same name. A folder too
          -- long for what is left loses its front: the end of it is the part
          -- nearest the file.
          --
          -- A grep hit is the other way about: where it was found is muted,
          -- and the line it found is what is read, and loses its end.
          hitRow = isJust (itemLine item)
          name = rowLead item
          base = T.length (itemText item) - T.length name
          folder = rowTrail item
          folderCell = max (rsNameCells sc + 3) (T.length name + 3)
          (folderShown, dropped)
            | hitRow = (T.take (cells - folderCell) folder, 0)
            | otherwise = clipFront (cells - folderCell) folder
          matched k = U.elem k pos
          nameColor k
            | hitRow = tcMuted tc
            | matched (base + k) = themeYellow theme
            | otherwise = tcName tc
          folderColor k
            | matched (k + dropped) = themeYellow theme
            | hitRow = tcName tc
            | otherwise = tcMuted tc
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

-- | What a row shows first: a file's name, or the file and line a grep hit is
-- at.
rowLead :: Item -> Text
rowLead item = case itemLine item of
  Nothing -> fileNameOf (itemText item)
  Just ln -> T.pack (takeFileName (itemPath item)) <> ":" <> T.pack (show (ln + 1))

-- | What a row shows beside that: the folder a file is in, or the line a grep
-- hit is on.
rowTrail :: Item -> Text
rowTrail item = case itemLine item of
  Just _ -> full
  Nothing -> if base > 0 then T.take (base - 1) full else ""
  where
    full = itemText item
    base = T.length full - T.length (fileNameOf full)

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
runsOf txt colorAt =
  [ (i, T.pack (map snd run), colorAt i)
  | run@((i, _) : _) <- runs
  ]
  where
    -- Chars stay together for as long as their colours answer the same.
    runs =
      groupBy
        (\(a, _) (b, _) -> colorToWord32 (colorAt a) == colorToWord32 (colorAt b))
        (zip [0 :: Int ..] (T.unpack txt))

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
  | otherwise = cellGlyphs x0 y cellW font col txt
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
-- status bar says them of the file that is open. The path is worked out from
-- the file rather than taken from the row, which for a grep hit is a line of
-- code.
previewHeading :: Ui :> es => Picker -> Eff es ()
previewHeading pk =
  rowWith (padXY 10 6 . tight . fillW . gap 12 . alignMid) $ case currentItem pk of
    Nothing -> labelWith (tight . fontMuted . alignMid) " "
    Just item -> do
      let full = relative (pkRoot pk) (itemPath item)
          name = fileNameOf full
          folder = T.dropEnd (T.length name) full
          pv = pkPreview pk
          n = pvTotal pv
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

-- | The lines of the file, in a scroller of nano-ui's on both axes: the
-- wheel, Ctrl+D and Ctrl+U move it, and its scrollbars say where in the file
-- the view is. A preview that has just been read opens on what it is about:
-- the top of a file, or the line a grep found, and far enough along that line
-- to show what was found. What stands in for the lines -- a file that could
-- not be read -- is text that wraps.
previewBody :: Ui :> es => FontMetrics -> Float -> Picker -> Eff es Picker
previewBody fm cellW pk0
  | not (T.null (pvNote pv)) = scope $ do
      columnWith (padXY pickerPad 6 . tight . grow . fillH) $
        void (richTextWith (fillW . fontMuted) [inlineText (pvNote pv)])
      pure pk0
  | otherwise = scope $ do
      inp <- askInput
      sid <- currentId
      metrics <- getScrollMetricsUi sid
      let lineH = codeHeight fm
          lns = pvLines pv
          count = sizeofSmallArray lns
          digits = max 2 (length (show (max 1 (pvFirst pv + count))))
          gutterW = fromIntegral (digits + 2) * cellW
          widest = foldl' (\m (PreviewLine t _ _) -> max m (T.length t)) 0 lns
          Rect _ _ viewW viewH = maybe (Rect 0 0 400 400) scrollViewport metrics
          contentW = max viewW (gutterW + fromIntegral widest * cellW + pickerPad)
          contentH = max viewH (fromIntegral count * lineH)
          mods = inputModifiers inp
          ctrl = modCtrl mods && not (modAlt mods)
          chord c = ctrl && T.any (== c) (inputChars inp)
          -- Where a preview that has just been read opens: the hit a third of
          -- the way down, and the first thing found on it a third of the way
          -- along, when it is further along than the view reaches.
          hitAt = case pvHit pv of
            Just ln | ln - pvFirst pv >= 0 && ln - pvFirst pv < count -> Just (ln - pvFirst pv)
            _ -> Nothing
          openY = maybe 0 (\i -> max 0 ((fromIntegral i - viewH / lineH / 3) * lineH)) hitAt
          openX = case hitAt of
            Just i
              | PreviewLine _ _ ((a, b) : _) <- indexSmallArray lns i
              , gutterW + fromIntegral b * cellW > viewW ->
                  max 0 (fromIntegral a * cellW - (viewW - gutterW) / 3)
            _ -> 0
      when (pvScroll pv < 0) (setScrollOffsetUi sid (V2 openX openY))
      -- The preview is read rather than walked, so it is the wheel and the
      -- chords a pager scrolls with that move it, and never the arrows:
      -- those are the rows'.
      when (chord 'd') (scrollPagesUi sid (V2 0 0.5) ScrollInstant)
      when (chord 'u') (scrollPagesUi sid (V2 0 (-0.5)) ScrollInstant)
      setScrollStepUi sid (3 * lineH)
      offX <- maybe 0 (v2X . scrollOffset) <$> getScrollMetricsUi sid
      let scene =
            CodeScene
              { csLines = lns
              , csBase = pvFirst pv
              , csLineH = lineH
              , csTextH = fmLineHeight fm
              , csCellW = cellW
              , csGutterW = gutterW
              , csGutterAt = clamp 0 (max 0 (contentW - viewW)) offX
              , csHit = pvHit pv
              }
      _ <-
        scrollArea2D (tight . gap 0 . grow . fillH) $
          customWidget
            defaultCustomWidgetSpec
              { widgetLayout = fixedWH contentW contentH defaultLayout
              , widgetDraw = \cdc r -> drawCode cdc scene r
              , widgetContent = contentKeyOf [keyPart (pvVersion pv), keyPart (csGutterAt scene)]
              , widgetCursor = Just (const UiCursorDefault)
              , widgetDamageSlop = 0
              }
      pure pk0 {pkPreview = pv {pvScroll = 0}}
  where
    pv = pkPreview pk0

-- | Everything the preview's drawing reads.
data CodeScene = CodeScene
  { csLines :: !(SmallArray PreviewLine)
  , csBase :: !Int
  -- ^ The line of the file the first of the lines is.
  , csLineH :: !Float
  , csTextH :: !Float
  , csCellW :: !Float
  , csGutterW :: !Float
  , csGutterAt :: !Float
  -- ^ How far along the lines the view is scrolled, which is where the
  -- numbers are drawn so that they stay down its left.
  , csHit :: !(Maybe Int)
  }

-- | The draw ops of the preview: every line of the file that was read, in the
-- colours the editor sets them in, and the numbers down the left of the view
-- over whatever is scrolled under them. The scroller shows the part of it in
-- view and moves it without its ops being built again.
drawCode :: CustomDrawContext -> CodeScene -> Rect -> SmallArray DrawOp
drawCode _ sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect colBackground : concatMap lineOps [0 .. count - 1] ++ gutter)
  where
    lns = csLines sc
    count = sizeofSmallArray lns
    lineH = csLineH sc
    cellW = csCellW sc
    gutterW = csGutterW sc
    lineY ln = y + fromIntegral ln * lineH
    textY ry = ry + (lineH - csTextH sc) / 2
    font kind = TextFont pickerFontSize FontMono (tokenWeight kind) FontStyleNormal DecorationNone
    textX = x + gutterW
    gx = x + csGutterAt sc
    isHit ln = csHit sc == Just (csBase sc + ln)

    lineOps ln =
      let PreviewLine txt spans found = indexSmallArray lns ln
          ry = lineY ln
          band = [FillRect (Rect x ry w lineH) colCurrentLine | isHit ln]
          -- What the grep found, marked as Ctrl+F marks a match in the
          -- editor.
          matches =
            [ FillRect (Rect (textX + fromIntegral a * cellW) ry (fromIntegral (b - a) * cellW) lineH) colFindMatch
            | (a, b) <- found
            , b > a
            ]
       in band ++ matches ++ spanOps (textY ry) 0 txt spans

    gutter = FillRect (Rect gx y gutterW h) colGutter : map number [0 .. count - 1]
    number ln =
      let num = T.pack (show (csBase sc + ln + 1))
       in DrawTextStyled
            (gx + gutterW - cellW - fromIntegral (T.length num) * cellW)
            (textY (lineY ln))
            (font TokPlain)
            num
            (if isHit ln then colGutterActive else colGutterText)

    spanOps _ _ _ [] = []
    spanOps ry !cell txt (Span n kind : rest) =
      cellRun (textX + fromIntegral cell * cellW) ry cellW (font kind) (tokenColor kind) (T.take n txt)
        ++ spanOps ry (cell + n) (T.drop n txt) rest

