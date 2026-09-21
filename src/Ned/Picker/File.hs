-- | The files under a root, for the finder to match a query against.
--
-- The walk runs on the finder's gathering thread and hands the files over as
-- it finds them, nearest the root first, so that what is usually wanted is
-- there to pick before a walk of a hundred thousand files has finished.
module Ned.Picker.File
  ( fileSource
  , skippedDirs
  ) where

import Control.Monad (void, when)
import Data.Char (toLower)
import Data.List (sortOn)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as U
import Ned.Picker.Source
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))

-- | Every file under the root, nearest the root first.
fileSource :: Source
fileSource =
  Source
    { srcTitle = "Find File"
    , srcPrompt = "Find a file\x2026"
    , srcGather = \root _query sink -> walkFiles root sink
    , srcLive = False
    }

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
-- anybody opens, and all of them hold thousands. A grep passes them over too,
-- so that it looks through the files the finder would offer and no others.
--
-- The rest of a dotted directory is walked, so that the workflows under
-- @.github@ are found like anything else.
skippedDirs :: [FilePath]
skippedDirs =
  [ ".git"
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

skipDir :: FilePath -> Bool
skipDir name = map toLower name `elem` skippedDirs

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

    item path = Item {itemText = relative root path, itemPath = path, itemLine = Nothing, itemMarks = U.empty, itemRanges = []}

    flush [] = pure True
    flush batch = feed (reverse batch)

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
