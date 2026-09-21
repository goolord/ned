-- | Direct bindings to the bundled fzf matcher and to ned's own bulk-scan
-- shim over it. Pointers are unmanaged and the slab is mutable scratch space:
-- prefer "Ned.Fuzzy", which owns both with finalizers.
module Ned.Fuzzy.Raw
  ( -- * Opaque C types
    FzfPattern
  , FzfSlab
  , FzfPosition

    -- * Case handling
  , CaseMode (..)
  , caseModeCode

    -- * Patterns
  , fzf_parse_pattern
  , fzf_free_pattern
  , p_fzf_free_pattern

    -- * Scratch space
  , fzf_make_default_slab
  , fzf_free_slab
  , p_fzf_free_slab

    -- * Matching
  , fzf_get_score
  , fzf_get_positions
  , fzf_free_positions
  , ned_fzf_positions_data
  , ned_fzf_positions_size
  , ned_fzf_match_many
  ) where

import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..), CSize (..))
import Foreign.Ptr (FunPtr, Ptr)

-- | A parsed query. One query is many terms: a space separates AND terms,
-- @|@ separates the OR alternatives within one, and @^@, @$@, @'@ and @!@
-- mark a term as prefix-anchored, suffix-anchored, exact or inverted.
data FzfPattern

-- | Reusable scratch buffers for the matcher's dynamic-programming tables.
-- A slab is written during a match, so one match at a time per slab.
data FzfSlab

-- | A matched-offset array returned by 'fzf_get_positions'.
data FzfPosition

-- | How a query's case is read.
data CaseMode
  = -- | Case-insensitive until the query contains an upper-case character.
    CaseSmart
  | CaseIgnore
  | CaseRespect
  deriving (Eq, Show, Enum, Bounded)

-- | The @fzf_case_types@ value for a mode.
caseModeCode :: CaseMode -> CInt
caseModeCode = fromIntegral . fromEnum

-- | @fzf_parse_pattern case_mode normalize pattern fuzzy@. The pattern string
-- is copied, so the caller keeps ownership of it.
foreign import ccall unsafe "fzf.h fzf_parse_pattern"
  fzf_parse_pattern :: CInt -> CBool -> CString -> CBool -> IO (Ptr FzfPattern)

foreign import ccall unsafe "fzf.h fzf_free_pattern"
  fzf_free_pattern :: Ptr FzfPattern -> IO ()

foreign import ccall unsafe "fzf.h &fzf_free_pattern"
  p_fzf_free_pattern :: FunPtr (Ptr FzfPattern -> IO ())

foreign import ccall unsafe "fzf.h fzf_make_default_slab"
  fzf_make_default_slab :: IO (Ptr FzfSlab)

foreign import ccall unsafe "fzf.h fzf_free_slab"
  fzf_free_slab :: Ptr FzfSlab -> IO ()

foreign import ccall unsafe "fzf.h &fzf_free_slab"
  p_fzf_free_slab :: FunPtr (Ptr FzfSlab -> IO ())

-- | The candidate's score against the pattern; @0@ when it does not match.
-- An empty pattern scores every candidate @1@.
foreign import ccall unsafe "fzf.h fzf_get_score"
  fzf_get_score :: CString -> Ptr FzfPattern -> Ptr FzfSlab -> IO Int32

-- | The byte offsets the pattern matched, @NULL@ for no match and for an
-- empty pattern. Free the result with 'fzf_free_positions'.
foreign import ccall unsafe "fzf.h fzf_get_positions"
  fzf_get_positions :: CString -> Ptr FzfPattern -> Ptr FzfSlab -> IO (Ptr FzfPosition)

foreign import ccall unsafe "fzf.h fzf_free_positions"
  fzf_free_positions :: Ptr FzfPosition -> IO ()

foreign import ccall unsafe "ned_fzf.h ned_fzf_positions_data"
  ned_fzf_positions_data :: Ptr FzfPosition -> IO (Ptr Word32)

foreign import ccall unsafe "ned_fzf.h ned_fzf_positions_size"
  ned_fzf_positions_size :: Ptr FzfPosition -> IO CSize

-- | Score a whole candidate arena in one call. See @cbits/ned_fzf.h@ for the
-- arena layout; the result is the number of candidates that matched, of which
-- the first @limit@ are written to the output arrays, best first.
--
-- A scan over a large list runs for milliseconds, which an @unsafe@ call
-- would spend with the capability's other Haskell threads and the garbage
-- collector held off, so this one is @safe@.
foreign import ccall safe "ned_fzf.h ned_fzf_match_many"
  ned_fzf_match_many ::
    Ptr FzfPattern ->
    Ptr FzfSlab ->
    CString ->
    Ptr Word32 ->
    CSize ->
    CSize ->
    Ptr Word32 ->
    Ptr Int32 ->
    IO CSize
