-- |
-- Module      : Ned.Fuzzy
-- Description : fzf's fuzzy matching over a list of candidates
-- License     : MIT
--
-- fzf's matching algorithm, bundled as C and wrapped for Haskell. A query is
-- 'compile'd once and then scored against many candidates; scoring reports
-- both a ranking score and, for the rows actually drawn, which characters the
-- query matched, which is what a picker underlines or colours.
--
-- A filter over a fixed list of candidates, such as a file finder, builds a
-- 'Candidates' arena once and rescans it on every keystroke:
--
-- > cands <- pure (candidates paths)
-- > slab <- newSlab
-- > pat <- compile (defaultQuery "wid/tex")
-- > hits <- matchCandidates slab pat cands 5000
-- > for_ (U.toList (matchedIndices hits)) $ \i ->
-- >   T.putStrLn (candidateText cands i)
--
-- = Queries
--
-- A query is fzf's extended syntax: space-separated terms all have to match,
-- @|@ separates alternatives inside a term, and @^x@, @x$@, @'x@ and @!x@
-- anchor a term to the start, to the end, or make it an exact or an excluded
-- substring.
--
-- = Threads
--
-- A 'Slab' is mutable scratch space, so one match at a time per slab: give
-- each thread that matches its own. 'compile' is not thread-safe either (the
-- C parser splits the query with @strtok@), so compile queries on one thread.
module Ned.Fuzzy
  ( -- * Scratch space
    Slab
  , newSlab

    -- * Queries
  , Query (..)
  , defaultQuery
  , CaseMode (..)
  , Pattern
  , compile

    -- * One candidate
  , score
  , matchPositions

    -- * A list of candidates
  , Candidates
  , candidates
  , candidatesCount
  , candidateText
  , candidateTexts
  , Matches (..)
  , noMatches
  , matchCandidates
  ) where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BU
import Data.Int (Int32)
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Data.Vector.Unboxed qualified as U
import Data.Word (Word32, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peekElemOff)
import Ned.Fuzzy.Raw

-- | Scratch buffers the matcher reuses between candidates. Make one per
-- matching thread and keep it: a slab is a hundred kilobytes, and allocating
-- one per query is most of the cost of a short query.
newtype Slab = Slab (ForeignPtr FzfSlab)

-- | Allocate a slab sized for fzf's default limits. It is freed when the
-- handle is collected.
newSlab :: IO Slab
newSlab = do
  p <- fzf_make_default_slab
  if p == nullPtr
    then ioError (userError "Ned.Fuzzy.newSlab: out of memory")
    else Slab <$> newForeignPtr p_fzf_free_slab p

-- | A query and how to read it.
data Query = Query
  { queryText :: !Text
  -- ^ The query itself, in fzf's extended syntax. An empty query matches
  -- every candidate with score 1 and keeps the candidates' own order.
  , queryCase :: !CaseMode
  , queryNormalize :: !Bool
  -- ^ Fold latin letters to their unaccented forms before matching, so that
  -- @cafe@ matches @café@.
  , queryFuzzy :: !Bool
  -- ^ 'False' makes every term a plain substring term, as fzf's @--exact@
  -- does.
  }
  deriving (Eq, Show)

-- | A smart-case fuzzy query with no normalisation: what a picker's prompt
-- usually wants.
defaultQuery :: Text -> Query
defaultQuery txt =
  Query
    { queryText = txt
    , queryCase = CaseSmart
    , queryNormalize = False
    , queryFuzzy = True
    }

-- | A compiled query, freed when it is collected. Compiling parses the
-- query's terms, so compile once per keystroke rather than once per
-- candidate.
newtype Pattern = Pattern (ForeignPtr FzfPattern)

-- | Parse a query. See the module header on thread safety.
compile :: Query -> IO Pattern
compile q =
  B.useAsCString (TE.encodeUtf8 (queryText q)) $ \cs -> do
    p <-
      fzf_parse_pattern
        (caseModeCode (queryCase q))
        (cbool (queryNormalize q))
        cs
        (cbool (queryFuzzy q))
    if p == nullPtr
      then ioError (userError "Ned.Fuzzy.compile: out of memory")
      else Pattern <$> newForeignPtr p_fzf_free_pattern p

cbool :: Bool -> CBool
cbool b = if b then 1 else 0

-- | How well the candidate matches: @0@ for no match, higher is better. The
-- numbers only mean something against each other, and only for one query.
--
-- The matcher reads C strings, so a candidate is matched up to its first NUL
-- character.
score :: Slab -> Pattern -> Text -> IO Int
score slab pat txt =
  withCandidate txt $ \cs ->
    withPair slab pat $ \pp sp -> fromIntegral <$> fzf_get_score cs pp sp

-- | Which characters of the candidate the query matched, as character
-- offsets into the 'Text', ascending. Empty when the query does not match, and
-- also for an empty query, which matches everything and highlights nothing.
--
-- This costs about as much as one 'score', so ask for the rows on screen
-- rather than for every match.
matchPositions :: Slab -> Pattern -> Text -> IO (U.Vector Int)
matchPositions slab pat txt = do
  let bytes = TE.encodeUtf8 txt
  raw <- withPositions slab pat bytes
  pure (toCharOffsets bytes raw)

-- Run an action with a candidate as a NUL-terminated C string.
withCandidate :: Text -> (CString -> IO a) -> IO a
withCandidate txt = B.useAsCString (TE.encodeUtf8 txt)

withPair :: Slab -> Pattern -> (Ptr FzfPattern -> Ptr FzfSlab -> IO a) -> IO a
withPair (Slab slabFp) (Pattern patFp) act =
  withForeignPtr patFp $ \pp -> withForeignPtr slabFp $ \sp -> act pp sp

-- The matched byte offsets, ascending and without repeats. fzf appends them
-- in the order its traceback walks the candidate, which is neither.
withPositions :: Slab -> Pattern -> ByteString -> IO (U.Vector Int)
withPositions slab pat bytes =
  B.useAsCString bytes $ \cs ->
    withPair slab pat $ \pp sp -> do
      posPtr <- fzf_get_positions cs pp sp
      if posPtr == nullPtr
        then pure U.empty
        else do
          dataPtr <- ned_fzf_positions_data posPtr
          n <- fromIntegral <$> ned_fzf_positions_size posPtr
          offsets <-
            if dataPtr == nullPtr || n <= (0 :: Int)
              then pure []
              else mapM (fmap fromIntegral . peekElemOff dataPtr) [0 .. n - 1]
          fzf_free_positions posPtr
          pure (U.fromList (dedup (sort offsets)))
  where
    dedup (x : y : rest) | x == y = dedup (y : rest)
    dedup (x : rest) = x : dedup rest
    dedup [] = []

-- | Byte offsets into a UTF-8 buffer as character offsets into the text it
-- decodes to. ASCII, which paths and code usually are, maps through
-- unchanged.
toCharOffsets :: ByteString -> U.Vector Int -> U.Vector Int
toCharOffsets bytes offsets
  | U.null offsets = offsets
  | B.all (< 0x80) bytes = offsets
  | otherwise = U.map charAt offsets
  where
    -- The character index of a byte is the number of characters that start
    -- before it: every byte that is not a UTF-8 continuation byte starts one.
    table :: U.Vector Int
    table =
      U.scanl' (\acc b -> if isContinuation b then acc else acc + 1) 0 $
        U.fromListN (B.length bytes) (B.unpack bytes)
    charAt i
      | i < 0 = 0
      | i < U.length table = U.unsafeIndex table i
      | otherwise = U.last table

isContinuation :: Word8 -> Bool
isContinuation b = b .&. 0xC0 == 0x80

-- | A list of candidates laid out for the matcher: the texts, and the same
-- texts as one NUL-separated UTF-8 buffer that a whole scan runs over without
-- coming back to Haskell.
--
-- Building one copies every candidate, so build it when the list changes, not
-- when the query does.
data Candidates = Candidates
  { candTexts :: !(V.Vector Text)
  , candArena :: !ByteString
  , candOffsets :: !(S.Vector Word32)
  -- ^ Candidate starts, plus one past the last candidate's NUL.
  }

-- | Lay out a list of candidates. A candidate is matched up to its first NUL
-- character, as the C matcher reads it.
candidates :: V.Vector Text -> Candidates
candidates texts =
  Candidates
    { candTexts = texts
    , candArena = arena
    , candOffsets = S.fromListN (V.length texts + 1) (scanOffsets 0 encoded)
    }
  where
    encoded = map TE.encodeUtf8 (V.toList texts)
    arena = BL.toStrict (BB.toLazyByteString (foldMap (\b -> BB.byteString b <> BB.word8 0) encoded))
    scanOffsets !acc [] = [acc]
    scanOffsets !acc (b : rest) = acc : scanOffsets (acc + fromIntegral (B.length b) + 1) rest

-- | How many candidates there are.
candidatesCount :: Candidates -> Int
candidatesCount = V.length . candTexts

-- | The candidate at an index, which a 'Matches' reports.
candidateText :: Candidates -> Int -> Text
candidateText cands i = candTexts cands V.! i

-- | Every candidate, in the order they were laid out in.
candidateTexts :: Candidates -> V.Vector Text
candidateTexts = candTexts

-- | The result of a scan: how many candidates matched, and the best of them.
-- 'matchedIndices' and 'matchedScores' are the same length and in the same
-- order, best match first, and index into the 'Candidates' that was scanned.
data Matches = Matches
  { matchedTotal :: !Int
  -- ^ Candidates that matched at all, which the scan's limit does not cap.
  , matchedIndices :: !(U.Vector Int)
  , matchedScores :: !(U.Vector Int)
  }
  deriving (Eq, Show)

-- | No candidate matched.
noMatches :: Matches
noMatches = Matches 0 U.empty U.empty

-- | Score every candidate and take the best @limit@ of them. Ties go to the
-- shorter candidate and then to the earlier one, so equally good matches keep
-- the list's own order.
--
-- The limit bounds the result, not the work: 'matchedTotal' still counts every
-- match, so a picker can show @48/12043@ while holding only the rows it can
-- scroll through.
matchCandidates :: Slab -> Pattern -> Candidates -> Int -> IO Matches
matchCandidates slab pat cands limit
  | n <= 0 || cap <= 0 = pure noMatches
  | otherwise = do
      idxBuf <- SM.new cap
      scoreBuf <- SM.new cap
      total <-
        BU.unsafeUseAsCString (candArena cands) $ \arena ->
          S.unsafeWith (candOffsets cands) $ \offs ->
            SM.unsafeWith idxBuf $ \outIdx ->
              SM.unsafeWith scoreBuf $ \outScore ->
                withPair slab pat $ \pp sp ->
                  ned_fzf_match_many
                    pp
                    sp
                    arena
                    offs
                    (fromIntegral n)
                    (fromIntegral cap)
                    outIdx
                    outScore
      idxs <- S.unsafeFreeze idxBuf
      scores <- S.unsafeFreeze scoreBuf
      let written = min cap (fromIntegral total)
      pure
        Matches
          { matchedTotal = fromIntegral total
          , matchedIndices = U.generate written (fromIntegral . S.unsafeIndex idxs)
          , matchedScores = U.generate written (intScore . S.unsafeIndex scores)
          }
  where
    n = candidatesCount cands
    cap = max 0 (min n limit)
    intScore :: Int32 -> Int
    intScore = fromIntegral
