-- | The names in a ctags file, for completing a word with.
--
-- A tags file is what @ctags -R@ writes: a line a name, then the file it is
-- in and how to find it there, a tab between each. The names are kept sorted
-- without their case, so the ones a word starts are found by a binary search
-- and read off in order, whatever case the word was typed in.
--
-- The file is looked for in the folder the tree is on and every folder above
-- it, as vim's @tags;@ looks, and read again whenever it changes. The reading
-- is done on a thread of its own, since the tags of a large tree run to a
-- million lines.
module Ned.Complete.Tags
  ( Tags
  , noTags
  , tagCount
  , parseTags
  , tagSource
  , findTagsFile
  , watchTags
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, evaluate, try)
import Data.Char (isUpper)
import Data.List (sortOn)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Ned.Complete (Candidate (..), Source, answers)
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath (takeDirectory, takeFileName, (</>))

-- | A name, the name without its case, and the name of the file it is in.
data Tag = Tag !Text !Text !Text

-- | The names of a tags file, each once, sorted without their case.
newtype Tags = Tags (V.Vector Tag)

noTags :: Tags
noTags = Tags V.empty

tagCount :: Tags -> Int
tagCount (Tags v) = V.length v

-- | The names in a tags file's bytes. The lines of its header, which start
-- with @!_@, are not names. A name in more than one file is kept once, under
-- the first of them.
parseTags :: BS.ByteString -> Tags
parseTags bytes = Tags (V.fromList (dedupe (sortOn (\(Tag name folded _) -> (folded, name)) tags)))
  where
    tags = [tag | l <- BC.lines bytes, not ("!_" `BS.isPrefixOf` l), Just tag <- [parseLine l]]
    parseLine l = case BS.split 9 l of
      raw : file : _ | not (BS.null raw) ->
        let name = TE.decodeUtf8Lenient raw
         in Just (Tag name (fold name) (T.pack (takeFileName (BC.unpack file))))
      _ -> Nothing
    -- A name already in lower case, as most are, is its own folded form and
    -- takes no more room.
    fold name = if T.any isUpper name then T.toLower name else name
    dedupe (a@(Tag x _ _) : rest@(Tag y _ _ : _))
      | x == y = dedupe (a : drop 1 rest)
      | otherwise = a : dedupe rest
    dedupe short = short

-- | The names a word starts, each under the name of its file.
tagSource :: Tags -> Source
tagSource (Tags v) typed
  | T.null typed = []
  | otherwise =
      [ Candidate name file
      | Tag name _ file <- takeWhile (\(Tag _ f _) -> folded `T.isPrefixOf` f) (V.toList (V.drop (lowerBound 0 (V.length v)) v))
      , answers typed name
      ]
  where
    folded = T.toLower typed
    -- The first name that does not sort before the word.
    lowerBound lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
              Tag _ f _ = v V.! mid
           in if f < folded then lowerBound (mid + 1) hi else lowerBound lo mid

-- | The tags file in a folder, or in the nearest folder above it that has one.
findTagsFile :: FilePath -> IO (Maybe FilePath)
findTagsFile dir = do
  let here = dir </> "tags"
  found <- doesFileExist here
  let up = takeDirectory dir
  case () of
    _
      | found -> pure (Just here)
      | up == dir -> pure Nothing
      | otherwise -> findTagsFile up

-- | Keep the names of the tags file for a folder handed over as it changes;
-- this never returns. It looks for the file every two seconds, which is a
-- stat a folder up to the root, and reads it again when it is another file,
-- or the same file written since. A file that goes away takes its names with
-- it.
watchTags :: FilePath -> ((Tags -> Tags) -> IO ()) -> IO ()
watchTags root update = go Nothing
  where
    go seen = do
      now <- stamp
      case now of
        _ | now == seen -> pure ()
        Nothing -> update (const noTags)
        Just (path, _) ->
          try @IOException (BS.readFile path) >>= \case
            Left _ -> update (const noTags)
            Right bytes -> do
              -- Read here, on this thread, and not by the frame that first
              -- asks for a name.
              let Tags v = parseTags bytes
              _ <- evaluate (V.foldl' (\n (Tag _ _ _) -> n + 1) (0 :: Int) v)
              update (const (Tags v))
      threadDelay 2000000
      go now
    stamp =
      findTagsFile root >>= \case
        Nothing -> pure Nothing
        Just path -> either (const Nothing) (Just . (path,)) <$> try @IOException (getModificationTime path)
