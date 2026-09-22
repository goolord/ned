-- | What a finder looks through: the rows it shows, and where they come from.
--
-- This is the whole of what a source and the finder agree on. The finder in
-- "Ned.Picker" knows nothing of a disk or a process; a source knows nothing
-- of a window. The files under a root are "Ned.Picker.File", and a live grep
-- over them is "Ned.Picker.Grep".
module Ned.Picker.Source
  ( Item (..)
  , Sink
  , Source (..)
  , GatherFailed (..)
  , relative
  ) where

import Control.Exception (Exception (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import System.FilePath (makeRelative)

-- | One row: the text it is drawn and matched by, and what picking it opens.
data Item = Item
  { itemText :: !Text
  -- ^ What the query is matched against, and what the row shows.
  , itemPath :: !FilePath
  , itemLine :: !(Maybe Int)
  -- ^ The line the row is about, counted from zero. A file has none; a grep
  -- hit is the line it was found on, which is where the preview opens.
  , itemMarks :: !(U.Vector Int)
  -- ^ Which characters of the text answered the query, when the source
  -- answered it itself. A source the matcher filters leaves this empty and
  -- the matcher says instead.
  , itemRanges :: ![(Int, Int)]
  -- ^ Where on its line a hit answered the query, from a character to the
  -- one past it, counted along the line as it is in the file. The preview
  -- marks these as the editor marks what Ctrl+F found. A file has none.
  }

-- | Where a gatherer hands its rows over, a batch at a time. It answers
-- 'False' when the picker has moved on -- another query, or the picker put
-- away -- and a gatherer that is told so stops. An empty batch hands nothing
-- over and only asks, which is how a gatherer that has found nothing for a
-- while finds out whether it is still wanted.
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
  , srcPrompt :: !Text
  -- ^ What the empty prompt says, which is what to type into it.
  , srcGather :: !(FilePath -> Text -> Sink -> IO ())
  -- ^ Find the rows under a root for a query, feeding them to the sink. It
  -- runs on a thread of its own, so it may take as long as it likes, and an
  -- exception it throws is what the finder says in place of the rows.
  , srcLive :: !Bool
  -- ^ Whether the query is the gatherer's to answer. A live source is
  -- gathered again whenever the query settles and is not filtered afterwards.
  }

-- | Why a gatherer found nothing, in words the finder can put where the rows
-- would be: a program it needs that is not there, say. Anything else a
-- gatherer throws is said as it comes.
newtype GatherFailed = GatherFailed Text
  deriving (Show)

instance Exception GatherFailed where
  displayException (GatherFailed msg) = T.unpack msg

-- | A path as the rows show it: where it sits under the root, written with
-- forward slashes whatever the platform separates with, since that is how a
-- path is typed at the prompt.
relative :: FilePath -> FilePath -> Text
relative root path = T.replace "\\" "/" (T.pack (makeRelative root path))
