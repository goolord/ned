-- | A live grep: every line under a root that answers the query, found by
-- ripgrep.
--
-- The finder hands the query over once typing has paused, and @rg --json@ is
-- run on it. What comes back is a message a line -- a file begun, a line that
-- matched, a file ended -- and each line that matched is a row, with the
-- characters ripgrep matched on it marked. A query that changes while a
-- search is running is noticed within a tenth of a second, whether or not the
-- search has found anything, and the search is killed.
module Ned.Picker.Grep
  ( grepSource
  , grepHit
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (void, when)
import Data.Aeson.Micro (FromJSON (..), Object, Parser, decodeStrict, withObject, (.:), (.:?))
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as U
import Ned.Picker.File (skippedDirs)
import Ned.Picker.Source
import System.Exit (ExitCode (..))
import System.IO (hIsEOF, hSetBinaryMode)
import System.IO.Error (isDoesNotExistError)
import System.Process (terminateProcess)
import System.Process.Typed
  ( createPipe
  , getStderr
  , getStdout
  , nullStream
  , proc
  , setStderr
  , setStdin
  , setStdout
  , unsafeProcessHandle
  , waitExitCode
  , withProcessTerm
  )

-- | Every line under the root that answers the query, file by file.
grepSource :: Source
grepSource =
  Source
    { srcTitle = "Search Files"
    , srcPrompt = "Search the files\x2026"
    , srcGather = grep
    , srcLive = True
    }

-- | How many lines a search offers before it stops. A query of one letter
-- matches most of the lines of a tree, and nobody scrolls through them all.
grepLimit :: Int
grepLimit = 10000

-- | The most of a line a row keeps. A minified file is one line a megabyte
-- long, and no row is a megabyte wide.
grepCells :: Int
grepCells = 400

-- | What ripgrep is asked: JSON, whatever the case, and the files the file
-- finder would offer -- the hidden ones too, but not what a build or a version
-- control system leaves behind, whatever the case of its name, as the finder
-- skips them. A ripgrep configuration file the reader keeps for the terminal
-- is not read, since what is in it is not what the finder asks for. Its
-- complaints about files it could not read are kept to itself: the finder
-- shows what was found.
grepArgs :: FilePath -> Text -> [String]
grepArgs root query =
  ["--json", "--no-config", "--ignore-case", "--hidden", "--no-messages", "--glob-case-insensitive"]
    <> concat [["--glob", '!' : d <> "/"] | d <- skippedDirs]
    <> ["--", T.unpack query, root]

-- | Run ripgrep on a query, and hand its rows over as it finds them. Nothing
-- is looked for until something is typed.
grep :: FilePath -> Text -> Sink -> IO ()
grep root query sink
  | T.all isSpace query = pure ()
  | otherwise = do
      -- The editor is a window, and ripgrep a console program. Windows puts
      -- one up for it unless every one of its standard handles is one the
      -- editor gave it, so what it reads is the null device rather than
      -- nothing at all, and what it complains to is a pipe the editor reads.
      let config =
            setStdin nullStream
              . setStdout createPipe
              . setStderr createPipe
              $ proc "rg" (grepArgs root query)
      started <- try $ withProcessTerm config $ \p -> do
        let out = getStdout p
        hSetBinaryMode out True
        -- A search that finds nothing hands nothing over, so it is never
        -- refused a batch and would run on after the finder has moved on.
        -- This asks after it, and stops ripgrep when the answer is no, which
        -- ends the output that is being read below.
        let watch = do
              threadDelay 100000
              wanted <- sink []
              if wanted then watch else void (try @SomeException (terminateProcess (unsafeProcessHandle p)))
        -- Whether the output was read to its end, rather than left because
        -- the finder moved on or enough was found.
        let go :: Int -> [Item] -> IO Bool
            go !found batch
              | found >= grepLimit = False <$ flush batch
              | otherwise =
                  hIsEOF out >>= \case
                    True -> flush batch
                    False -> do
                      msg <- decodeStrict <$> BC.hGetLine out
                      case msg of
                        Just (Hit item) -> go (found + 1) (item : batch)
                        -- A file's hits go over together when it is
                        -- finished, so that a search that finds a line now
                        -- and then shows it without waiting on the rest, and
                        -- the preview of a file has every hit in it to mark.
                        Just FileEnd -> flush batch >>= \ok -> if ok then go found [] else pure False
                        _ -> go found batch
        finished <- bracket (forkIO watch) killThread (\_ -> go 0 [])
        -- ripgrep that could not search at all -- a pattern it cannot read,
        -- most often -- says why, and that is said in place of the rows.
        when finished $ do
          complaint <- BS.hGetContents (getStderr p)
          code <- waitExitCode p
          when (code == ExitFailure 2 && not (BS.null complaint)) $
            throwIO (GatherFailed (complaintOf complaint))
      -- Leaving 'withProcessTerm' kills ripgrep, which is how a search the
      -- finder has moved on from is stopped.
      case started of
        Right () -> pure ()
        Left e
          | isDoesNotExistError e -> throwIO (GatherFailed "Searching needs ripgrep (rg) on the PATH.")
          | otherwise -> throwIO e
  where
    flush = sink . reverse

-- | What ripgrep said went wrong, from the last line of its complaint: a
-- pattern it cannot read ends with @error: unclosed group@, say.
complaintOf :: BS.ByteString -> Text
complaintOf bytes = "ripgrep cannot search for this: " <> T.strip (fromMaybe final (T.stripPrefix "error:" final))
  where
    said = filter (not . T.null) (map T.strip (T.lines (TE.decodeUtf8Lenient bytes)))
    final = if null said then "" else last said

-- | What of ripgrep's messages the search reads: a line that matched, and the
-- end of a file.
data Message = Hit !Item | FileEnd | Other

instance FromJSON Message where
  parseJSON = withObject "message" $ \o ->
    o .: "type" >>= \case
      ("match" :: Text) -> maybe Other Hit <$> (o .: "data" >>= parseHit)
      "end" -> pure FileEnd
      _ -> pure Other

-- | One line of ripgrep's JSON as a row, if it is a line that matched.
grepHit :: BS.ByteString -> Maybe Item
grepHit bytes = case decodeStrict bytes of
  Just (Hit item) -> Just item
  _ -> Nothing

-- | A match: the file, the line and where on it. A path or a line that is not
-- UTF-8 comes as bytes rather than text, and is not a row.
parseHit :: Object -> Parser (Maybe Item)
parseHit d = do
  path <- d .: "path" >>= (.:? "text")
  line <- d .: "lines" >>= (.:? "text")
  number <- d .: "line_number"
  subs <- d .: "submatches" >>= traverse (\s -> (,) <$> s .: "start" <*> s .: "end")
  pure (hitItem <$> path <*> line <*> number <*> pure subs)

-- | A row for a line that matched: the line as it reads, without its indent
-- or its line ending, with the characters ripgrep matched marked.
--
-- ripgrep says where its matches are in bytes, and a row marks characters, so
-- each is counted over again in the line it is in. The matches come in order
-- along the line, so each is counted on from the one before: a minified line
-- with a match in every word is walked once, and not once a match.
hitItem :: Text -> Text -> Int -> [(Int, Int)] -> Item
hitItem path raw number subs =
  Item
    { itemText = shown
    , itemPath = T.unpack path
    , itemLine = Just (number - 1)
    , itemMarks = U.fromList [k - indent | (a, b) <- takeWhile ((< end) . fst) ranges, k <- [max a indent .. min b end - 1]]
    , itemRanges = ranges
    }
  where
    line = T.dropWhileEnd (\c -> c == '\n' || c == '\r') raw
    utf8 = TE.encodeUtf8 line
    ranges = snd (mapAccumL range (0, 0) subs)
    range at (s, e) =
      let (at', a) = seek at s
          (at'', b) = seek at' e
       in (at'', (a, b))
    -- The character a byte starts, from the byte and character the count has
    -- reached. One out of order is counted again from the start of the line.
    seek at@(!byte, !char) o
      | o <= 0 = (at, 0)
      | o < byte = seek (0, 0) o
      | otherwise =
          let char' = char + chars (BS.take (o - byte) (BS.drop byte utf8))
           in ((o, char'), char')
    -- A character is counted at its first byte, which is any byte that does
    -- not carry one on.
    chars = BS.foldl' (\n w -> if w .&. 0xC0 /= 0x80 then n + 1 else n) (0 :: Int)
    indent = T.length (T.takeWhile isSpace line)
    -- A tab is a cell, like any other character a row shows, so that a
    -- mark stays on the character it marks.
    shown = T.map (\c -> if c < ' ' then ' ' else c) (T.take grepCells (T.drop indent line))
    end = indent + T.length shown
