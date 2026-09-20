-- | The file tree beside the editor: the folder the open file is in, with its
-- directories opening and closing and a click on a file opening it.
--
-- Like the editor, it is a nano-ui custom widget that scrolls by itself and
-- whose draw ops are keyed on everything it reads, so a frame in which
-- nothing changed builds nothing. A directory is read the first time it is
-- opened and kept, so the tree holds what has been looked at and no more: a
-- folder nobody opened is never listed, however large it is.
module Ned.FileTree
  ( FileTree (..)
  , Row (..)
  , newFileTree
  , setRoot
  , parentRoot
  , hasParentRoot
  , reveal
  , refresh
  , collapseAll
  , rootName
  , fileTreePanel
  , defaultTreeWidth
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, type (:>))
-- 'Row' here is a row of the tree, not nano-ui's layout direction.
import NanoUI hiding (Row)
import NanoUI.Context (Context (..), getPrevRect)
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Monad (askContext, askInput)
import Ned.Highlight (langName, languageFor)
import Ned.View (caretColor)
import Ned.Widget
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (equalFilePath, normalise, takeDirectory, takeFileName, (</>))

--------------------------------------------------------------------------------
-- State
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
  | -- | The bar between the tree and the editor, this far from its left edge.
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

--------------------------------------------------------------------------------
-- Look
--------------------------------------------------------------------------------

-- The tree is chrome, so it takes its colours from the theme, as the menu bar
-- and the status bar do: the window's own colour, which is a step darker than
-- the editor's background, and the panel's text on it.
--
-- It spends one colour of its own, on the row whose file the editor has open,
-- and that colour is the caret's ('caretColor'). Everything else here is grey:
-- a hue in this window means "you are here", and it means nothing else.
--
-- What tells the rows apart is weight, not colour. A folder is set semibold,
-- and takes the foreground while it is open and the muted grey while it is
-- shut, so that brightness says which folder's contents you are looking at. A
-- file is set normal and always at full strength: files are what a pointer is
-- aimed at, so they are the brightest thing in the panel, and the folders
-- around them are scaffolding to scan past.

treePad, treeIndent, treeChevron, treeIcon, treeBarW, treeMark :: Float
treePad = 8
treeIndent = 14
treeChevron = 14

-- | The column an icon sits in, between the chevron and the name. A file has
-- no chevron but still leaves room for one, so that the icons of a folder and
-- of the files inside it line up in a column.
treeIcon = 18
treeBarW = 10

-- | The bar down the left of the row whose file the editor has open. It is its
-- own mark and not the selection's, because arrowing through the tree moves
-- the selection away and the open file has to stay findable.
treeMark = 3

-- | The only corner in the window, and the scrollbar thumb's own. It goes on
-- the things a pointer grabs or picks and on nothing else; the caret, the
-- selection bands and the rules are all square.
treeRadius :: Float
treeRadius = 3

splitterW :: Float
splitterW = 5

-- | A row is scanned rather than read, so it sits tighter than a line of text.
rowHeight :: FontMetrics -> Float
rowHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 2

--------------------------------------------------------------------------------
-- The widget
--------------------------------------------------------------------------------

-- | The panel: the root's name, the rows, and the bar that resizes it. Pass
-- the tree and keep the result; the response is for hanging a context menu
-- on, and the path is a file the rows were asked to open.
--
-- It takes the keyboard when @wantFocus@ is set, which the application does
-- while the tree is the thing last clicked on.
fileTreePanel :: Ui :> es => Bool -> Maybe FilePath -> FileTree -> Eff es (Response, FileTree, Maybe FilePath)
fileTreePanel wantFocus current ft0 =
  rowWith (tight . gap 0 . fillH) $ do
    (resp, ft1, opened) <- columnWith (tight . gap 0 . fillH . fixedW (ftWidth ft0)) $ do
      -- The padding goes on last: 'tight' before it would take it off again,
      -- and the name is meant to start where the rows' own names do. It is
      -- set at full strength and semibold: it names the thing the panel is
      -- about, and muted grey had it reading as a row that could not be
      -- clicked.
      rowWith (padXY (treePad + treeChevron + 2) 6 . tight . fillW . gap 4 . alignMid) $
        labelWith (tight . fontSemiBold) (rootName ft0)
      separator
      treeRows wantFocus current ft0
    ft2 <- splitterBar ft1
    pure (resp, ft2, opened)

-- | The rows, in one widget that scrolls itself.
treeRows :: Ui :> es => Bool -> Maybe FilePath -> FileTree -> Eff es (Response, FileTree, Maybe FilePath)
treeRows wantFocus current ft0 = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  let fm = ctxFontMetrics ctx
      lineH = rowHeight fm
  prev <- uiIO (getPrevRect ctx wid)
  let rect = fromMaybe (Rect 0 0 (ftWidth ft0) 600) prev
      viewRows = realToFrac (rectH rect / lineH) :: Double

  -- The tree keeps the keyboard for as long as it is the thing being used, as
  -- the editor does with its own.
  when wantFocus $ uiIO (takeFocus ctx wid)

  let mouse = inputMousePos inp
      inside = rectContains rect mouse
      localY = v2Y mouse - rectY rect
      rowsNow = ftRows ft0
      rowCount = sizeofSmallArray rowsNow
      overBar = inside && v2X mouse >= rectX rect + rectW rect - treeBarW && fromIntegral rowCount > viewRows
      bar = scroller rect rowCount viewRows
      pointedRow = floor (ftScroll ft0 + realToFrac (localY / lineH)) :: Int

      pressedNow = (inputMousePressed inp || inputMouseRightPressed inp) && inside

      -- The pointer. A press on a folder opens or closes it, and one on a
      -- file opens the file.
      (ftMouse, openedByMouse)
        | inputMousePressed inp && overBar =
            let grab = thumbGrab bar (ftScroll ft0) localY
             in (ft0 {ftDrag = DragThumb grab, ftScroll = thumbScroll bar grab localY}, Nothing)
        | pressedNow =
            case rowAt rowsNow pointedRow of
              Nothing -> (ft0, Nothing)
              Just hit ->
                let picked = ft0 {ftSelected = Just (rowPath hit)}
                 in if inputMouseRightPressed inp
                      then (picked, Nothing)
                      else
                        if rowDir hit
                          then (toggle (rowPath hit) picked, Nothing)
                          else (picked, Just (rowPath hit))
        | not (inputMouseDown inp) =
            (case ftDrag ft0 of DragThumb _ -> ft0 {ftDrag = DragNone}; _ -> ft0, Nothing)
        | otherwise = case ftDrag ft0 of
            DragThumb grab -> (ft0 {ftScroll = thumbScroll bar grab localY}, Nothing)
            _ -> (ft0, Nothing)

  -- nano-ui runs a frame for a pointer that only moved when it came over
  -- another widget. Every row here is the same widget, so a pointer crossing
  -- from one row to the next asks for nothing, and the row drawn under it
  -- would stay where it was until something else wanted a frame: in an editor
  -- that sleeps between caret blinks, half a second. While the pointer is
  -- over the tree it asks for its own frames. A frame whose rows have not
  -- changed builds no draw ops and repaints nothing, so this costs the wake
  -- and no more, and it stops as soon as the pointer leaves.
  when inside (wakeAfter 0.03)

  -- A directory opened by this frame's click is read before the frame draws it.
  ftLoaded <- uiIO (loadPending ftMouse)

  -- The keyboard, when the tree has it.
  let (ftKeys, openedByKey)
        | not wantFocus = (ftLoaded, Nothing)
        | otherwise = foldInputKeys applyKey (ftLoaded, Nothing) (inputKeys inp)

      -- The wheel, three rows a notch.
      V2 _ wheelY = if inside then inputScroll inp else V2 0 0
      scrolled = ftScroll ftKeys + realToFrac wheelY * 3

      -- Follow the selection when it was just asked for, and then keep the
      -- scroll within what there is to show.
      rows1 = ftRows ftKeys
      count1 = sizeofSmallArray rows1
      sel1 = selectedRow ftKeys
      follow y = if ftReveal ftKeys && sel1 >= 0 then followRow sel1 viewRows y else y
      scroll1 = clamp 0 (maxScroll count1 viewRows) (follow scrolled)

      ft1 =
        ftKeys
          { ftScroll = scroll1
          , -- The row asked for is either on screen now or was never there to
            -- find; either way the ask is done with, unless a directory above
            -- it is still to be read.
            ftReveal = ftReveal ftKeys && sel1 < 0 && not (null (ftPending ftKeys))
          , ftPressed = pressedNow
          }
      opened = maybe openedByKey Just openedByMouse

      hovered = if inside && not overBar then floor (scroll1 + realToFrac (localY / lineH)) else -1
      scene =
        Scene
          { scRows = rows1
          , scVersion = ftVersion ft1
          , scScroll = scroll1
          , scLineH = lineH
          , scViewRows = viewRows
          , scSelected = ftSelected ft1
          , scCurrent = current
          , scHovered = if hovered >= 0 && hovered < count1 then hovered else -1
          , scFocused = wantFocus
          , scThumbHot = overBar || isThumb (ftDrag ft1)
          }

  (resp, ()) <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (grow . fillH) defaultLayout
        , widgetDraw = \cdc r -> drawTree cdc scene r
        , widgetContent = sceneKey scene
        , widgetCursor = Just (const UiCursorDefault)
        , widgetFocusable = True
        , widgetDamageSlop = 0
        }
  pure (resp, ft1, opened)
  where
    isThumb = \case DragThumb _ -> True; _ -> False

    -- Up and down walk the rows, left closes a folder or steps out to the one
    -- above, right opens one or steps into it, and Enter opens a file.
    applyKey (ft, op) k =
      let rows = ftRows ft
          n = sizeofSmallArray rows
          here = selectedRow ft
          sel = rowAt rows here
       in case k of
            KeyUp -> (selectRow (if here < 0 then n - 1 else here - 1) ft, op)
            KeyDown -> (selectRow (if here < 0 then 0 else here + 1) ft, op)
            KeyHome -> (selectRow 0 ft, op)
            KeyEnd -> (selectRow (n - 1) ft, op)
            KeyLeft -> case sel of
              Just r | rowOpen r -> (toggle (rowPath r) ft, op)
              Just r -> (selectRow (parentOf rows here (rowDepth r)) ft, op)
              Nothing -> (ft, op)
            KeyRight -> case sel of
              Just r | rowDir r && not (rowOpen r) -> (toggle (rowPath r) ft, op)
              Just r | rowDir r -> (selectRow (here + 1) ft, op)
              _ -> (ft, op)
            KeyEnter -> case sel of
              Just r | rowDir r -> (toggle (rowPath r) ft, op)
              Just r -> (ft, Just (rowPath r))
              Nothing -> (ft, op)
            _ -> (ft, op)

    -- The row the one at @i@ sits under: the first one above it that is a
    -- step shallower.
    parentOf rows i depth = go (i - 1)
      where
        go j
          | j < 0 = -1
          | rowDepth (indexSmallArray rows j) < depth = j
          | otherwise = go (j - 1)

rowAt :: SmallArray Row -> Int -> Maybe Row
rowAt rows i
  | i < 0 || i >= sizeofSmallArray rows = Nothing
  | otherwise = Just (indexSmallArray rows i)

maxScroll :: Int -> Double -> Double
maxScroll rowCount viewRows = max 0 (fromIntegral rowCount - viewRows)

-- | The view and its scrollbar, over so many rows.
scroller :: Rect -> Int -> Double -> Scroller
scroller rect rowCount viewRows =
  Scroller (rectH rect) (viewRows / max 1 (fromIntegral rowCount)) (maxScroll rowCount viewRows)

-- | The bar between the tree and the editor, which drags to resize it.
splitterBar :: Ui :> es => FileTree -> Eff es FileTree
splitterBar ft0 = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  prev <- uiIO (getPrevRect ctx wid)
  let rect = fromMaybe (Rect 0 0 splitterW 600) prev
      mouse = inputMousePos inp
      over = rectContains rect mouse
      ft1
        | inputMousePressed inp && over = ft0 {ftDrag = DragWidth (v2X mouse - rectX rect)}
        | not (inputMouseDown inp) = case ftDrag ft0 of
            DragWidth _ -> ft0 {ftDrag = DragNone}
            _ -> ft0
        | otherwise = case ftDrag ft0 of
            -- The layout follows a frame behind, so the width moves by what
            -- the pointer is from where it took the bar, and settles there.
            DragWidth grab ->
              ft0 {ftWidth = clamp minTreeWidth maxTreeWidth (ftWidth ft0 + (v2X mouse - rectX rect - grab))}
            _ -> ft0
      hot = over || isWidth (ftDrag ft1)
  _ <-
    customWidgetWithId
      wid
      defaultCustomWidgetSpec
        { widgetLayout = (fillH . fixedW splitterW) defaultLayout
        , widgetDraw = \cdc (Rect x y w h) ->
            let theme = cdcTheme cdc
             in smallArrayFromList
                  -- The strip carries on the editor's page, and the seam sits
                  -- at the tree's own edge rather than down the middle of the
                  -- strip the pointer grabs, so the panel ends where it looks
                  -- like it ends. Taking hold of it brightens the seam; it
                  -- stays grey, because the one hue in this window is spoken
                  -- for.
                  [ FillRect (Rect x y w h) (styleBg (themePanel theme))
                  , FillRect (Rect x y 1 h) (if hot then themeMuted theme else themeSeparator theme)
                  ]
        , widgetContent = contentKey [if hot then 1 else 0]
        , widgetCursor = Just (const UiCursorEwResize)
        , widgetDamageSlop = 0
        }
  pure ft1
  where
    isWidth = \case DragWidth _ -> True; _ -> False

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------

-- | Everything the drawing reads.
data Scene = Scene
  { scRows :: !(SmallArray Row)
  , scVersion :: !Int
  , scScroll :: !Double
  , scLineH :: !Float
  , scViewRows :: !Double
  , scSelected :: !(Maybe FilePath)
  , scCurrent :: !(Maybe FilePath)
  , scHovered :: !Int
  , scFocused :: !Bool
  , scThumbHot :: !Bool
  }

-- | A number that changes when what the tree draws does. The version stands
-- for the rows, which are worked out only when they change.
sceneKey :: Scene -> Int
sceneKey sc =
  contentHash
    [ scVersion sc
    , round (scScroll sc * 64)
    , hashText (maybe "" T.pack (scSelected sc))
    , hashText (maybe "" T.pack (scCurrent sc))
    , scHovered sc
    , fromEnum (scFocused sc)
    , fromEnum (scThumbHot sc)
    ]

-- | The draw ops of the rows on screen, and of the scrollbar over them when
-- there is more than the view holds.
drawTree :: CustomDrawContext -> Scene -> Rect -> SmallArray DrawOp
drawTree cdc sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect (themeWindow theme) : concatMap rowOps [first .. last'] ++ bar)
  where
    theme = cdcTheme cdc
    fm = cdcFont cdc
    surface = themePanel theme
    lineH = scLineH sc
    rows = scRows sc
    count = sizeofSmallArray rows
    first = max 0 (floor (scScroll sc))
    last' = min (count - 1) (first + ceiling (h / lineH))
    yOff = realToFrac (fromIntegral first - scScroll sc) * lineH
    rowY i = y + yOff + fromIntegral (i - first) * lineH
    textY ry = ry + (lineH - fmLineHeight fm) / 2
    font weight = TextFont 0 FontRegular weight FontStyleNormal DecorationNone
    -- The lane the scrollbar has, which is nothing until there is more to
    -- show than the view holds.
    lane = if fromIntegral count > scViewRows sc then treeBarW else 0
    -- The separator, most of the way back toward the window behind it. A
    -- rule this quiet is enough to follow down a column, and a deep tree
    -- draws one of them for every level, so it has to stay under the names.
    colSpine = lerpColor (themeWindow theme) (themeSeparator theme) 0.8
    colChevron = themeMuted theme
    -- Three rungs of the one grey ladder. The row the keyboard is on has to
    -- stay apart from the row the pointer is merely over, and it dims rather
    -- than disappears when the keyboard goes elsewhere, so that arrowing back
    -- lands where you left off. None of them is tinted: 'themeSelection' is
    -- built from the theme's accent, and a blue slab under the aqua name of
    -- the open file put two hues in a panel that is allowed one.
    colHover = lerpColor (themeWindow theme) (styleHoverBg surface) 0.45
    colPickedAway = styleHoverBg surface
    colPicked = lerpColor (themeSeparator theme) (styleFg surface) 0.1
    -- 'scrollBarThumbColor' comes out at 1.5 to 1 on this panel, which is not
    -- a thumb you can find. These are 2.5 and 3.6 to one, to sit beside the
    -- editor's own bar rather than disappear next to it.
    colThumb = lerpColor (themeWindow theme) (styleFg surface) 0.3
    colThumbHot = lerpColor (themeWindow theme) (styleFg surface) 0.42

    -- The tint on a file's page is the family of language the editor would
    -- open it as, which is the one thing about a file the tree already knows
    -- and the name does not always say. There are far more languages than
    -- there are colours in the theme, so they share by family rather than
    -- each having one of their own; prose and anything unrecognised stay
    -- grey. The tint is on the icon alone -- the names stay in the grey
    -- ladder -- so the panel reads as a column of chips beside a list, and
    -- not as a list in a dozen colours.
    tintFor path = case langName (languageFor path) of
      l | l `elem` ["Haskell", "Cabal", "Nix"] -> themePurple theme
      l | l `elem` ["C", "C++", "C#", "Rust", "Go", "Zig", "Java"] -> themeOrange theme
      l | l `elem` ["JavaScript", "TypeScript", "Python", "Lua", "Shell", "PowerShell", "SQL"] -> themeYellow theme
      l | l `elem` ["JSON", "TOML", "YAML", "XML", "CSS", "HTML", "Dockerfile", "Makefile"] -> themeGreen theme
      "Markdown" -> themeRed theme
      _ -> themeMuted theme

    rowOps i =
      let r = indexSmallArray rows i
          ry = rowY i
          indent = treePad + fromIntegral (rowDepth r) * treeIndent
          selected = maybe False (equalFilePath (rowPath r)) (scSelected sc)
          -- The file the editor has, which is a path the application made and
          -- not one of ours, so it is matched the way the platform would.
          isCurrent = maybe False (equalFilePath (rowPath r)) (scCurrent sc)
          -- What a pick lands on is inset rather than run from edge to edge,
          -- so the panel keeps a margin down both sides and the mark has
          -- somewhere of its own to sit.
          pick = Rect (x + treeMark + 2) (ry + 1) (max 0 (w - treeMark - 4 - lane)) (lineH - 2)
          backdrop
            | selected = [FillRoundedRect pick treeRadius (if scFocused sc then colPicked else colPickedAway)]
            | scHovered sc == i = [FillRoundedRect pick treeRadius colHover]
            | otherwise = []
          -- One rule for each folder this row sits inside, down the middle of
          -- that folder's own chevron. This is what makes the rows a tree
          -- rather than a list of names: at the top level there is one root
          -- and so no rule at all.
          spine =
            [ FillRect (Rect (rule k) ry 1 lineH) colSpine
            | k <- [0 .. rowDepth r - 1]
            ]
          rule k = fromIntegral (round (x + treePad + fromIntegral k * treeIndent + treeChevron / 2) :: Int)
          mark = [FillRect (Rect x (ry + 1) treeMark (lineH - 2)) caretColor | isCurrent]
          -- A folder has a chevron pointing along or down; a file has none.
          cy = ry + lineH / 2
          chevron
            | not (rowDir r) = []
            | otherwise =
                let cx = x + indent + treeChevron / 2
                 in [ if rowOpen r
                        then FillTriangle (cx - 5) (cy - 2.5) (cx + 5) (cy - 2.5) cx (cy + 3.5) colChevron
                        else FillTriangle (cx - 2.5) (cy - 5) (cx - 2.5) (cy + 5) (cx + 3.5) cy colChevron
                    ]
          icon
            | rowDir r = folderIcon (x + indent + treeChevron) cy (if rowOpen r then styleFg surface else themeMuted theme)
            | otherwise = fileIcon (x + indent + treeChevron) cy (tintFor (rowPath r))
          tx = x + indent + treeChevron + treeIcon
          avail = w - (tx - x) - treePad - lane
          (weight, color)
            | isCurrent = (WeightSemiBold, caretColor)
            | rowDir r = (WeightSemiBold, if rowOpen r then styleFg surface else themeMuted theme)
            | otherwise = (WeightNormal, styleFg surface)
       in backdrop ++ spine ++ mark ++ chevron ++ icon ++ [DrawTextStyled tx (textY ry) (font weight) (elide fm avail (rowName r)) color]

    bar
      | lane <= 0 = []
      | otherwise =
          let (thumbTop, thumbH) = thumbSpan (scroller rect count (scViewRows sc)) (scScroll sc)
           in [ FillRoundedRect
                  (Rect (x + w - treeBarW + 2) (y + thumbTop + 2) (treeBarW - 4) (thumbH - 4))
                  3
                  (if scThumbHot sc then colThumbHot else colThumb)
              ]

-- | The icons, built out of the shapes the toolkit has rather than loaded
-- from an image: two of them, because two is all the tree has to say. A
-- folder is a body under a tab. A file is a page with its top corner taken
-- off, which is the shape everyone reads as a document.
--
-- The corner is taken off rather than shaded over: the page is drawn as the
-- five-sided shape it ends up being, so what shows through the cut is
-- whatever the row behind it is, and one icon draws the same over a row that
-- is picked, hovered or plain. A shade would have to know the row's colour,
-- and at eleven pixels it did not read as a fold anyway.
--
-- Both are square-cornered. The one radius in the window belongs to the
-- things a pointer grabs or picks; an icon is neither.
--
-- Both are centred on @cy@, in a column @treeIcon@ wide starting at @ix@.
folderIcon :: Float -> Float -> Color -> [DrawOp]
folderIcon ix cy col =
  [ FillRect (Rect fx (cy - 5) 5 2) col
  , FillRect (Rect fx (cy - 3) 12 8) col
  ]
  where
    fx = fromIntegral (round ix :: Int) + 2

fileIcon :: Float -> Float -> Color -> [DrawOp]
fileIcon ix cy col =
  [ FillRect (Rect fx fy 5 4) col
  , FillRect (Rect fx (fy + 4) 9 8) col
  , FillTriangle (fx + 5) fy (fx + 9) (fy + 4) (fx + 5) (fy + 4) col
  ]
  where
    fx = fromIntegral (round ix :: Int) + 4
    fy = cy - 6

-- | A name cut to fit, with an ellipsis where it was cut.
elide :: FontMetrics -> Float -> Text -> Text
elide fm avail name
  | avail <= 0 = T.empty
  | full <= avail = name
  | otherwise = go (min (T.length name - 1) (max 1 (floor (avail / max 1 (full / fromIntegral (T.length name))))))
  where
    full = lineWidth fm name
    ellipsis = T.singleton '\x2026'
    go n
      | n <= 0 = ellipsis
      | lineWidth fm (T.take n name <> ellipsis) <= avail = T.take n name <> ellipsis
      | otherwise = go (n - 1)
