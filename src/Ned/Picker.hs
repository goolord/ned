-- | The fuzzy finder as data: a prompt, the rows that answer it, and a preview
-- of the one the keyboard is on.
--
-- A 'Source' gathers rows on a thread of its own and feeds them over as it
-- finds them; the editor's two are 'fileSource' and 'grepSource', the latter
-- answering the query itself ('srcLive') and asked again only once typing has
-- paused. Scoring is fzf's own, through "Ned.Fuzzy".
--
-- Nothing here draws. The panel the finder is, and what its keys and pointer
-- do to it, are "Ned.View.Picker"'s.
module Ned.Picker
  ( -- * The finder
    Picker (..)
  , openPicker
  , closePicker
  , restock
  , untilSettled
  , pickerSig

    -- * Its rows
  , hitCount
  , hitItem
  , currentItem
  , matchedChars

    -- * Its preview
  , Preview (..)
  , PreviewLine (..)
  , ensurePreview

    -- * What it looks through
  , Item (..)
  , Sink
  , Source (..)
  , GatherFailed (..)
  , fileSource
  , grepSource
  , relative
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, displayException, fromException, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Clock (getMonotonicTime)
import NanoUI (contentKeyOf, keyPart)
import qualified Ned.Fuzzy as Fuzzy
import Ned.Highlight
import Ned.Picker.File (fileSource)
import Ned.Picker.Grep (grepSource)
import Ned.Picker.Source
import Ned.Text (cellOfCol)
import System.IO (IOMode (ReadMode), withBinaryFile)

--------------------------------------------------------------------------------
-- The finder
--------------------------------------------------------------------------------

-- | The finder, between frames.
data Picker = Picker
  { pkSource :: !Source
  , pkRoot :: !FilePath
  , pkTyped :: !Text
  -- ^ The prompt; for a live source, ahead of the query.
  , pkQuery :: !Text
  -- ^ What the rows answer.
  , pkEditedAt :: !Double
  -- ^ When the prompt last changed, which for a live source is what says
  -- whether typing has paused.
  , pkSlab :: !Fuzzy.Slab
  , pkPattern :: !Fuzzy.Pattern
  -- ^ The query compiled, so the rows can ask which characters matched.
  , pkCell :: !(IORef Gathered)
  -- ^ Where the gatherer leaves its finds.
  , pkGen :: !Int
  , pkTaken :: !Int
  -- ^ How many gathered rows have been taken in.
  , pkDone :: !Bool
  -- ^ Whether the gatherer has found everything.
  , pkFailed :: !(Maybe Text)
  , pkStale :: !(Maybe Double)
  -- ^ While a new query is being gathered for, the rows on screen are the
  -- last query's, kept until the new one has something to put in their place
  -- or this moment has passed: emptying them at once would blank the list for
  -- as long as ripgrep takes to answer.
  , pkItems :: !(V.Vector Item)
  , pkCands :: !Fuzzy.Candidates
  -- ^ The rows laid out for the matcher.
  , pkHits :: !Fuzzy.Matches
  -- ^ What matched, best first, as indices.
  , pkCursor :: !Int
  -- ^ The hit the keyboard is on.
  , pkScroll :: !Double
  -- ^ The hit at the top of the list, and how far it is scrolled out.
  , pkHovered :: !Int
  -- ^ The hit the pointer is over, or -1.
  , pkToTop :: !Bool
  -- ^ Asks the next frame to scroll the rows back to the top.
  , pkNameCells :: !Int
  -- ^ How wide the column of names is. It only grows, so the folders beside
  -- it stay put.
  , pkPreview :: !Preview
  }

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
      , pkEditedAt = 0
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

-- | Put the finder away, and with it the gathering thread.
closePicker :: Picker -> IO ()
closePicker pk = writeIORef (pkCell pk) (Gathered (pkGen pk + 1) [] 0 True Nothing)

-- | A number that changes when anything the finder shows does, which is what
-- tells the application that another frame is wanted.
pickerSig :: Maybe Picker -> Int
pickerSig Nothing = 0
pickerSig (Just pk) =
  contentKeyOf
    [ keyPart (pkQuery pk), keyPart (pkTyped pk == pkQuery pk), keyPart (pkTaken pk), keyPart (hitCount pk)
    , keyPart (pkCursor pk), keyPart (pkScroll pk), keyPart (pkHovered pk), keyPart (pkDone pk)
    , keyPart (isJust (pkStale pk)), keyPart (pvVersion (pkPreview pk)), keyPart (pvScroll (pkPreview pk))
    ]

--------------------------------------------------------------------------------
-- Gathering
--------------------------------------------------------------------------------

-- | Start gathering for the query the picker holds; what an earlier gather
-- found stays on screen, marked stale, until 'harvest' has something to show.
gather :: Maybe Double -> Picker -> IO Picker
gather keepUntil pk = do
  let gen = pkGen pk + 1
  writeIORef (pkCell pk) (Gathered gen [] 0 False Nothing)
  void . forkIO $ do
    ran <- try (srcGather (pkSource pk) (pkRoot pk) (pkQuery pk) (batchSink (pkCell pk) gen))
    atomicModifyIORef' (pkCell pk) $ \g ->
      ( if gatGen g == gen
          then g {gatDone = True, gatFailed = either (Just . failure) (const Nothing) ran}
          else g
      , ()
      )
  pure pk {pkGen = gen, pkTaken = 0, pkDone = False, pkFailed = Nothing, pkStale = keepUntil}
  where
    failure e = maybe (T.pack (displayException e)) (\(GatherFailed msg) -> msg) (fromException e)

batchSink :: IORef Gathered -> Int -> Sink
batchSink cell gen batch = atomicModifyIORef' cell take'
  where
    take' g
      | gatGen g /= gen = (g, False)
      | null batch = (g, True)
      | otherwise = (g {gatBatches = batch : gatBatches g, gatCount = gatCount g + length batch}, True)

-- | Take in whatever the gatherer has found since the last frame, and answer
-- the query against it. The matcher is asked on every keystroke; a live
-- source is asked only when typing has paused ('settleDelay'), the prompt has
-- been emptied, or Enter was pressed, and what the old query found stays up
-- until the new one has rows, which is when the keyboard goes back to the
-- top. Rows that merely arrived leave it where it was.
restock :: Picker -> Text -> Bool -> IO Picker
restock pk0 typed submitted
  | srcLive (pkSource pk0) = do
      now <- getMonotonicTime
      let editedAt = if typed /= pkTyped pk0 then now else pkEditedAt pk0
          settled = submitted || T.null typed || now - editedAt >= settleDelay
      pk1 <-
        if settled && typed /= pkQuery pk0
          then gather (Just (now + 1)) pk0 {pkTyped = typed, pkQuery = typed, pkEditedAt = editedAt}
          else pure pk0 {pkTyped = typed, pkEditedAt = editedAt}
      pk <- harvest now pk1
      -- Stale rows stood down for rows of the new query is a new list: the
      -- keyboard goes back to the top.
      let fresh = isJust (pkStale pk1) && isNothing (pkStale pk)
      if fresh || pkTaken pk /= pkTaken pk1 then rematch fresh pk else pure pk
  | otherwise = do
      let changed = typed /= pkQuery pk0
          pk1 = pk0 {pkTyped = typed, pkQuery = typed}
      pk <- harvest 0 pk1
      if changed || pkTaken pk /= pkTaken pk1 then rematch changed pk else pure pk

-- | How long typing into a live source's prompt has to pause before the
-- query is asked.
settleDelay :: Double
settleDelay = 0.2

-- | How long until a live source's prompt, typed ahead of its query, settles:
-- the frame then is the one that asks it, and nothing else would wake for it.
untilSettled :: Picker -> IO (Maybe Double)
untilSettled pk
  | srcLive (pkSource pk) && pkTyped pk /= pkQuery pk = do
      now <- getMonotonicTime
      pure (Just (max 0 (pkEditedAt pk + settleDelay - now) + 0.001))
  | otherwise = pure Nothing

-- | Fold the batches the gatherer has left into the rows the matcher scans.
-- Stale rows are kept until the gatherer has handed some over, or has
-- finished, or has run out of the time it was given.
harvest :: Double -> Picker -> IO Picker
harvest now pk = taken <$> readIORef (pkCell pk)
  where
    taken g
      -- Another gather is running; what this one left is not ours.
      | gatGen g /= pkGen pk = pk
      | keepStale = pk
      -- Rows kept for a query still being gathered are committed whatever
      -- the gatherer has: one that finishes with nothing clears them, rather
      -- than leaving the last query's rows standing in for none.
      | gatCount g == pkTaken pk, isNothing (pkStale pk) = settled
      | otherwise =
          settled {pkItems = items, pkCands = Fuzzy.candidates (V.map itemText items), pkTaken = gatCount g}
      where
        keepStale = maybe False (\until_ -> gatCount g == 0 && not (gatDone g) && now < until_) (pkStale pk)
        settled = pk {pkDone = gatDone g, pkFailed = gatFailed g, pkStale = Nothing}
        items = V.fromList (concat (reverse (gatBatches g)))

-- | Score every row against the query and keep the best. A new query puts the
-- keyboard back on the first row; rows merely arriving leave it where it was.
rematch :: Bool -> Picker -> IO Picker
rematch fresh pk = do
  pattern_ <- Fuzzy.compile (Fuzzy.defaultQuery (matchQuery pk))
  hits <- Fuzzy.matchCandidates (pkSlab pk) pattern_ (pkCands pk) 20000
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
-- Its rows
--------------------------------------------------------------------------------

-- | How many rows answer the query.
hitCount :: Picker -> Int
hitCount = U.length . Fuzzy.matchedIndices . pkHits

-- | The item a row is of.
hitItem :: Picker -> Int -> Maybe Item
hitItem pk i
  | i < 0 || i >= hitCount pk = Nothing
  | otherwise = pkItems pk V.!? U.unsafeIndex (Fuzzy.matchedIndices (pkHits pk)) i

-- | The item the keyboard is on.
currentItem :: Picker -> Maybe Item
currentItem pk = hitItem pk (pkCursor pk)

-- | Which characters of an item answered the query. Asking costs about as
-- much as scoring the item again, so it is asked for the rows that are drawn
-- and not for the hundred thousand that are not.
matchedChars :: Picker -> Item -> IO (U.Vector Int)
matchedChars pk item
  | T.null (matchQuery pk) = pure (itemMarks item)
  | otherwise = Fuzzy.matchPositions (pkSlab pk) (pkPattern pk) (itemText item)

--------------------------------------------------------------------------------
-- Its preview
--------------------------------------------------------------------------------

-- | The file beside the rows, as far as it is read.
data Preview = Preview
  { pvOf :: !(Maybe FilePath)
  -- ^ What it is of, so a file is read once.
  , pvLines :: !(SmallArray PreviewLine)
  , pvWidest :: !Int
  -- ^ The widest of the lines, in cells.
  , pvNote :: !Text
  -- ^ What stands in for the lines.
  , pvHit :: !(Maybe Int)
  , pvFirst :: !Int
  -- ^ The line of the file the first of the lines is.
  , pvWhole :: !(V.Vector Text)
  -- ^ Every line read, for the other hits in the file.
  , pvMore :: !Bool
  -- ^ Whether the file goes on past what was read.
  , pvScroll :: !Double
  -- ^ Below zero until the preview is placed.
  , pvVersion :: !Int
  -- ^ Bumped when the lines change, for the content key.
  }

-- | A line of the preview, lexed when the file was read, with what a grep
-- found on it, from a cell to the one past it.
data PreviewLine = PreviewLine !Text ![Span] ![(Int, Int)]

emptyPreview :: Preview
emptyPreview = Preview Nothing emptySmallArray 0 "" Nothing 0 V.empty False 0 0

-- How much of a file the preview reads, and how many of its lines it keeps. A
-- hit is further in than the head of a file, so a preview of one reads more.
previewBytes, hitBytes, previewLines :: Int
previewBytes = 128 * 1024
hitBytes = 4 * 1024 * 1024
previewLines = 600

-- | Read the file the keyboard is on, if it is not the one already read. A
-- grep hit in the file that is up already is the lines round it, from what
-- was read of the file for the last one.
ensurePreview :: Picker -> IO Picker
ensurePreview pk = case currentItem pk of
  Nothing -> pure (if isNothing (pvOf pv0) then pk else bump emptyPreview)
  Just item
    | pvOf pv0 == Just (itemPath item) && pvHit pv0 == itemLine item -> pure pk
    | pvOf pv0 == Just (itemPath item) && not (V.null (pvWhole pv0)) ->
        pure (bump (windowPreview item (fileRanges pk (itemPath item)) (pvMore pv0) (pvWhole pv0)))
    | otherwise -> bump <$> readPreview item (fileRanges pk (itemPath item))
  where
    pv0 = pkPreview pk
    bump pv = pk {pkPreview = pv {pvVersion = pvVersion pv0 + 1}}

-- | Where a grep found what it was looking for in a file, line by line: every
-- hit in it that has been gathered, as Ctrl+F marks every match in view.
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
-- 'previewBytes' are read, so that a frame is not put on hold for a file the
-- reader may be arrowing straight past.
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
        -- A BOM is not a character of the first line, to the editor or to
        -- ripgrep, which counts a hit on that line from after it.
        bytes = fromMaybe got (BS.stripPrefix "\xEF\xBB\xBF" got)
        text = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') bytes
        -- A file longer than the read ends mid-line, and half a line shown
        -- whole says something the file does not.
        cut = BS.length got >= limit
  where
    note msg = (previewOf item) {pvNote = msg}
    dropLast False xs = xs
    dropLast True xs = if null xs then xs else init xs

-- | A preview of an item before anything is read of it; one nobody has
-- scrolled yet opens where the file is about.
previewOf :: Item -> Preview
previewOf item = emptyPreview {pvOf = Just (itemPath item), pvHit = itemLine item, pvScroll = -1}

-- | What the preview keeps of the lines that were read: the head of the file,
-- or the lines round a hit, lexed for colour as the editor lexes them, with
-- what a grep found marked. The lines round a hit are lexed from the first of
-- them, so a hit inside a long comment may be coloured as code.
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
        { pvLines = smallArrayFromList (snd (mapAccumL lexOne LexNormal (zip [from ..] (V.toList ls))))
        , pvWidest = V.foldl' (\w l -> max w (cellOfCol l (T.length l))) 0 ls
        , pvFirst = from
        }
  where
    kept = (previewOf item) {pvWhole = whole, pvMore = cut}
    total = V.length whole
    from = maybe 0 (\ln -> max 0 (min (total - previewLines) (ln - previewLines `div` 2))) (itemLine item)
    ls = V.slice from (min previewLines (total - from)) whole
    -- The state carries from one line to the next, as it does in the editor.
    lexOne st (ln, l) =
      let (spans, st') = lexLine (languageFor (itemPath item)) st l
       in (st', PreviewLine l spans [(cellOfCol l a, cellOfCol l b) | (a, b) <- IM.findWithDefault [] ln found])
