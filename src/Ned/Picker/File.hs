-- | The files under a root, for the finder to match a query against.
--
-- The walk runs on the finder's gathering thread and hands the files over
-- as it finds them, so that what is usually wanted is there to pick before
-- a walk of a hundred thousand files has finished.
module Ned.Picker.File
  ( fileSource
  , skippedDirs
  ) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import Data.Char (toLower)
import qualified Data.Vector.Unboxed as U
import Ned.Picker.Source
import System.Directory (doesDirectoryExist, pathIsSymbolicLink)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (takeFileName)

-- | Every file under the root.
fileSource :: Source
fileSource =
  Source
    { srcTitle = "Find File"
    , srcPrompt = "Find a file\x2026"
    , srcGather = \root _query sink -> walkFiles root sink
    , srcLive = False
    }

-- | How many files a walk offers, and how many it gathers before handing a
-- batch over. The batch is what makes the rows appear while the walk is
-- still running; the limit is what keeps a walk that wandered somewhere
-- enormous from running forever.
scanLimit, scanBatch :: Int
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

-- | The files under a root, handed over in batches as they are found. The
-- walk is dir-traverse's, which is lazy: the batches pull it along, and a
-- walk the finder has moved on from is abandoned where it stands, the rest
-- of it never asked for.
walkFiles :: FilePath -> Sink -> IO ()
walkFiles root feed =
  try (getDirFiltered wanted root >>= go 0 []) >>= \case
    Right () -> pure ()
    -- A directory that cannot be read ends the walk where it is, with what
    -- it has handed over so far.
    Left (_ :: IOException) -> pure ()
  where
    -- What the walk lists and where it goes on: a file is offered, and a
    -- directory is walked on unless the finder skips it or it is a link. A
    -- link leads somewhere that is either under the root already, and would
    -- be listed twice, or outside it, and is not what the finder was asked
    -- for; a link back up is neither, and would not end.
    wanted path = do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          link <- pathIsSymbolicLink path
          pure (not link && not (skipDir (takeFileName path)))
        else pure True

    go _found batch [] = void (feed (reverse batch))
    go !found batch (path : rest) = do
      isDir <- doesDirectoryExist path
      if isDir
        then go found batch rest
        else
          let found' = found + 1
              batch' = item path : batch
           in if length batch' >= scanBatch
                then do
                  ok <- feed (reverse batch')
                  if ok && found' < scanLimit
                    then go found' [] rest
                    else pure ()
                else go found' batch' rest

    item path = Item {itemText = relative root path, itemPath = path, itemLine = Nothing, itemMarks = U.empty, itemRanges = []}
