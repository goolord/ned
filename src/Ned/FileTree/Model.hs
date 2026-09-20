-- | The tree as data: what is open, what has been read, and the rows that
-- come of it.
--
-- A directory is read the first time it is opened and kept, so the tree holds
-- what has been looked at and no more: a folder nobody opened is never
-- listed, however large it is. The rows are worked out when the tree changes
-- rather than every frame, and 'ftVersion' says when they did, which is what
-- lets the drawing key itself on them cheaply.
--
-- Nothing here knows about nano-ui, the pointer or the keyboard. Every way
-- the tree can change is a function from 'FileTree' to 'FileTree', so the
-- panel in "Ned.FileTree" is left saying which one a frame calls for.
module Ned.FileTree.Model
  ( -- * The tree
    FileTree (..)
  , Row (..)
  , Drag (..)
  , newFileTree
  , rootName

    -- * Reading directories
  , loadPending

    -- * Moving about
  , setRoot
  , parentRoot
  , hasParentRoot
  , reveal
  , refresh
  , collapseAll
  , toggle

    -- * Rows
  , rowAt
  , selectedRow
  , selectRow

    -- * How wide the panel may be
  , defaultTreeWidth
  , minTreeWidth
  , maxTreeWidth
  ) where

import Control.Exception (SomeException, try)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (equalFilePath, normalise, takeDirectory, takeFileName, (</>))

--------------------------------------------------------------------------------
-- The tree
--------------------------------------------------------------------------------

-- | A row as it is drawn: one entry of a directory that is open.
data Row = Row
  { rowPath :: !FilePath
  , rowName :: !Text
  , rowDepth :: !Int
  -- ^ How many directories under the root it sits; the root's own entries are 0.
  , rowDir :: !Bool
  , rowOpen :: !Bool
  }
  deriving (Eq)

-- | What the pointer is holding, if anything.
data Drag
  = DragNone
  | -- | The scrollbar's thumb, this far below its top.
    DragThumb !Float
  | -- | The bar between the tree and the editor, this far from the tree's
    -- edge, which is where the width follows the pointer from.
    DragWidth !Float
  deriving (Eq)

data FileTree = FileTree
  { ftRoot :: !FilePath
  , ftOpen :: !(S.Set FilePath)
  -- ^ The directories drawn open. A directory in here that is not on screen
  -- stays open, so closing its parent and opening it again shows it as it was.
  , ftRead :: !(M.Map FilePath [(Text, Bool)])
  -- ^ What a directory held when it was read: each entry's name, and whether
  -- it is a directory. A directory that could not be read is in here empty,
  -- so it is not asked for again.
  , ftPending :: ![FilePath]
  -- ^ Directories that are drawn open and have not been read yet.
  , ftRows :: !(SmallArray Row)
  -- ^ The rows, worked out when the tree changes and not every frame.
  , ftVersion :: !Int
  -- ^ Bumped whenever the rows change, for the drawing's content key.
  , ftScroll :: !Double
  -- ^ The row at the top of the view; its fraction is how far it is scrolled out.
  , ftSelected :: !(Maybe FilePath)
  , ftReveal :: !Bool
  -- ^ Asks the next frame to scroll the selected row into view.
  , ftWidth :: !Float
  , ftDrag :: !Drag
  , ftPressed :: !Bool
  -- ^ Whether the pointer went down on the tree this frame.
  }

defaultTreeWidth, minTreeWidth, maxTreeWidth :: Float
defaultTreeWidth = 240
minTreeWidth = 120
maxTreeWidth = 640

-- | An empty tree on a directory, which the first frame reads.
newFileTree :: FilePath -> FileTree
newFileTree root =
  FileTree
    { ftRoot = root
    , ftOpen = S.empty
    , ftRead = M.empty
    , ftPending = [root]
    , ftRows = emptySmallArray
    , ftVersion = 0
    , ftScroll = 0
    , ftSelected = Nothing
    , ftReveal = False
    , ftWidth = defaultTreeWidth
    , ftDrag = DragNone
    , ftPressed = False
    }

-- | The name the header shows: the root's own, or the whole path when it is a
-- drive or a root directory, which has no name of its own.
rootName :: FileTree -> Text
rootName ft = if null name then T.pack (ftRoot ft) else T.pack name
  where
    name = takeFileName (ftRoot ft)

--------------------------------------------------------------------------------
-- The rows
--------------------------------------------------------------------------------

-- | Whether the platform takes two spellings of a name for the same file.
caseless :: Bool
caseless = equalFilePath "a" "A"

-- | The form a path is remembered under. A row's path is the root and the
-- names a directory listing gave; a path from outside can name the same file
-- another way, in another case where the platform does not mind, so what the
-- tree holds is matched by this and not by the letters.
pathKey :: FilePath -> FilePath
pathKey p = if caseless then map toLower (normalise p) else normalise p

-- | The rows of the tree as it stands: the root's entries, and under each
-- open directory its own, as far as what has been read goes.
rebuild :: FileTree -> FileTree
rebuild ft = ft {ftRows = smallArrayFromList (children 0 (ftRoot ft)), ftVersion = ftVersion ft + 1}
  where
    children depth dir =
      concat
        [ Row path name depth isDir open : if open then children (depth + 1) path else []
        | (name, isDir) <- M.findWithDefault [] (pathKey dir) (ftRead ft)
        , let path = dir </> T.unpack name
              open = isDir && S.member (pathKey path) (ftOpen ft)
        ]

-- | Read a directory: its entries, folders first and then files, each lot by
-- name. A directory that cannot be read (no permission, gone since) is empty.
readDir :: FilePath -> IO [(Text, Bool)]
readDir dir = do
  result <- try (listDirectory dir) :: IO (Either SomeException [FilePath])
  case result of
    Left _ -> pure []
    Right names -> do
      entries <- traverse (\n -> (,) (T.pack n) <$> isDir (dir </> n)) names
      pure (sortOn (\(n, d) -> (not d, T.toLower n, n)) entries)
  where
    isDir p = either (const False) id <$> (try (doesDirectoryExist p) :: IO (Either SomeException Bool))

-- | Read every directory that is drawn open and has not been read, until
-- there are none left: opening one brings its own open children into the
-- rows, and those have to be read as well.
loadPending :: FileTree -> IO FileTree
loadPending ft
  | null (ftPending ft) = pure ft
  | otherwise = do
      let todo = [d | d <- dedup (ftPending ft), not (M.member (pathKey d) (ftRead ft))]
      entries <- traverse (\d -> (,) (pathKey d) <$> readDir d) todo
      let ft' = rebuild ft {ftRead = foldl' (\m (d, es) -> M.insert d es m) (ftRead ft) entries, ftPending = []}
      loadPending ft' {ftPending = unread ft'}
  where
    -- One read for a directory named twice, however it was spelled.
    dedup = M.elems . M.fromList . map (\d -> (pathKey d, d))
    unread f = [rowPath r | r <- toList (ftRows f), rowOpen r, not (M.member (pathKey (rowPath r)) (ftRead f))]

--------------------------------------------------------------------------------
-- Moving about
--------------------------------------------------------------------------------

-- | Put the tree on another directory, keeping what has already been read.
setRoot :: FilePath -> FileTree -> FileTree
setRoot dir = rebuild . rootedAt dir

-- | 'setRoot', for a caller that works the rows out itself.
rootedAt :: FilePath -> FileTree -> FileTree
rootedAt dir ft = ft {ftRoot = dir, ftPending = dir : ftPending ft, ftScroll = 0}

-- | Whether the root has a directory above it to go to.
hasParentRoot :: FileTree -> Bool
hasParentRoot ft = not (equalFilePath (takeDirectory (ftRoot ft)) (ftRoot ft))

-- | Go up a directory, leaving the one just left open so the tree looks the
-- same with one level around it.
parentRoot :: FileTree -> FileTree
parentRoot ft
  | not (hasParentRoot ft) = ft
  | otherwise = setRoot (takeDirectory (ftRoot ft)) ft {ftOpen = S.insert (pathKey (ftRoot ft)) (ftOpen ft)}

-- | Show a file in the tree: open the directories above it, select it, and
-- have the next frame scroll to it. A file outside the root moves the root to
-- the folder it is in.
reveal :: FilePath -> FileTree -> FileTree
reveal path ft0 =
  rebuild
    ft
      { ftOpen = foldr (S.insert . pathKey) (ftOpen ft) dirs
      , ftPending = [d | d <- dirs, not (M.member (pathKey d) (ftRead ft))] ++ ftPending ft
      , ftSelected = Just path
      , ftReveal = True
      }
  where
    above = ancestors path
    ft = if any (equalFilePath (ftRoot ft0)) above then ft0 else rootedAt (takeDirectory path) ft0
    -- The directories between the file and the root, which is one of them.
    dirs = takeWhile (not . equalFilePath (ftRoot ft)) above

-- | The directories a path is under, nearest first, as far as the one that
-- has none above it. The count is a guard against a path that never gets
-- there.
ancestors :: FilePath -> [FilePath]
ancestors = take 64 . go . takeDirectory
  where
    go dir = dir : if equalFilePath (takeDirectory dir) dir then [] else go (takeDirectory dir)

-- | Forget what every directory held, so the next frame reads them again.
refresh :: FileTree -> FileTree
refresh ft = rebuild ft {ftRead = M.empty, ftPending = [ftRoot ft]}

-- | Close every directory.
collapseAll :: FileTree -> FileTree
collapseAll ft = rebuild ft {ftOpen = S.empty}

-- | Open or close a directory. One that has never been read is queued.
toggle :: FilePath -> FileTree -> FileTree
toggle path ft
  | S.member k (ftOpen ft) = rebuild ft {ftOpen = S.delete k (ftOpen ft)}
  | otherwise =
      rebuild
        ft
          { ftOpen = S.insert k (ftOpen ft)
          , ftPending = if M.member k (ftRead ft) then ftPending ft else path : ftPending ft
          }
  where
    k = pathKey path

-- | Where the selected path is among the rows, or @-1@.
selectedRow :: FileTree -> Int
selectedRow ft = maybe (-1) (`go` 0) (ftSelected ft)
  where
    rows = ftRows ft
    n = sizeofSmallArray rows
    go p i
      | i >= n = -1
      | equalFilePath (rowPath (indexSmallArray rows i)) p = i
      | otherwise = go p (i + 1)

-- | Select the row at an index, if there is one, and scroll to it.
selectRow :: Int -> FileTree -> FileTree
selectRow i ft
  | i < 0 || i >= sizeofSmallArray (ftRows ft) = ft
  | otherwise = ft {ftSelected = Just (rowPath (indexSmallArray (ftRows ft) i)), ftReveal = True}

rowAt :: SmallArray Row -> Int -> Maybe Row
rowAt rows i
  | i < 0 || i >= sizeofSmallArray rows = Nothing
  | otherwise = Just (indexSmallArray rows i)
