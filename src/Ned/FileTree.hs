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
import Data.Bits (xor)
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.IORef (writeIORef)
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
import NanoUI.Context (Context (..), getFocusId, getPrevRect)
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Monad (askContext, askInput)
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
setRoot dir ft = rebuild ft {ftRoot = dir, ftPending = dir : ftPending ft, ftScroll = 0}

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
    ft = if under (ftRoot ft0) path then ft0 else setRoot (takeDirectory path) ft0
    -- The directories between the file and the root. The root is reached
    -- because the file is under it; the count is a guard against a path that
    -- says otherwise.
    dirs = take 64 (takeWhile (not . equalFilePath (ftRoot ft)) (iterate takeDirectory (takeDirectory path)))

-- | Whether a path is somewhere under a directory.
under :: FilePath -> FilePath -> Bool
under root path = go (takeDirectory path) (64 :: Int)
  where
    go _ 0 = False
    go dir n
      | equalFilePath dir root = True
      | equalFilePath (takeDirectory dir) dir = False
      | otherwise = go (takeDirectory dir) (n - 1)

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

treePad, treeIndent, treeChevron, treeBarW :: Float
treePad = 6
treeIndent = 13
treeChevron = 14
treeBarW = 10

splitterW :: Float
splitterW = 5

rowHeight :: FontMetrics -> Float
rowHeight fm = fromIntegral (ceiling (fmLineHeight fm) :: Int) + 4

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
      -- and the name is meant to start where the rows' own names do.
      rowWith (padXY (treePad + treeChevron) 4 . tight . fillW . gap 4 . alignMid) $
        labelWith (tight . fontMuted) (rootName ft0)
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
  focus0 <- uiIO (getFocusId ctx)
  when (wantFocus && focus0 /= wid) $ uiIO $ do
    writeIORef (ctxFocusId ctx) wid
    writeIORef (ctxFocusVisible ctx) False

  let mouse = inputMousePos inp
      inside = rectContains rect mouse
      localY = v2Y mouse - rectY rect
      rowsNow = ftRows ft0
      rowCount = sizeofSmallArray rowsNow
      overBar = inside && v2X mouse >= rectX rect + rectW rect - treeBarW && fromIntegral rowCount > viewRows
      (thumbTop, thumbH) = thumbSpan rect rowCount viewRows (ftScroll ft0)
      pointedRow = floor (ftScroll ft0 + realToFrac (localY / lineH)) :: Int
      scrollToThumb grab =
        let range = maxScroll rowCount viewRows
            track = rectH rect - thumbH
         in if track <= 0 then 0 else realToFrac ((localY - grab) / track) * range

      pressedNow = (inputMousePressed inp || inputMouseRightPressed inp) && inside

      -- The pointer. A press on a folder opens or closes it, and one on a
      -- file opens the file.
      (ftMouse, openedByMouse)
        | inputMousePressed inp && overBar =
            let grab = if localY >= thumbTop && localY <= thumbTop + thumbH then localY - thumbTop else thumbH / 2
             in (ft0 {ftDrag = DragThumb grab, ftScroll = scrollToThumb grab}, Nothing)
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
            DragThumb grab -> (ft0 {ftScroll = scrollToThumb grab}, Nothing)
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
      follow y
        | not (ftReveal ftKeys) || sel1 < 0 = y
        | fromIntegral sel1 < y = fromIntegral sel1
        | fromIntegral sel1 + 1 > y + viewRows = fromIntegral sel1 + 1 - max 1 (fromIntegral (floor viewRows :: Int))
        | otherwise = y
      scroll1 = max 0 (min (maxScroll count1 viewRows) (follow scrolled))

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

-- | The scrollbar's thumb: its top and its height, within the widget.
thumbSpan :: Rect -> Int -> Double -> Double -> (Float, Float)
thumbSpan rect rowCount viewRows at =
  let h = rectH rect
      thumbH = max 28 (min h (h * realToFrac (viewRows / max 1 (fromIntegral rowCount))))
      range = maxScroll rowCount viewRows
      frac = if range <= 0 then 0 else realToFrac (at / range)
   in ((h - thumbH) * frac, thumbH)

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
              ft0 {ftWidth = max minTreeWidth (min maxTreeWidth (ftWidth ft0 + (v2X mouse - rectX rect - grab)))}
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
                  [ FillRect (Rect x y w h) (themeWindow theme)
                  , FillRect (Rect (x + w / 2 - 0.5) y 1 h) (if hot then themeAccent theme else themeSeparator theme)
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
  let fields =
        [ scVersion sc
        , round (scScroll sc * 64)
        , hashText (maybe "" T.pack (scSelected sc))
        , hashText (maybe "" T.pack (scCurrent sc))
        , scHovered sc
        , fromEnum (scFocused sc)
        , fromEnum (scThumbHot sc)
        ]
      h = foldl' (\acc v -> (acc `xor` v) * 1099511628211) 1469598103934665603 fields
   in if h == 0 then 1 else h
  where
    hashText = T.foldl' (\acc c -> (acc `xor` fromEnum c) * 1099511628211) 1469598103934665603

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
    font = TextFont 0 FontRegular WeightNormal FontStyleNormal DecorationNone

    rowOps i =
      let r = indexSmallArray rows i
          ry = rowY i
          indent = treePad + fromIntegral (rowDepth r) * treeIndent
          selected = maybe False (equalFilePath (rowPath r)) (scSelected sc)
          -- The file the editor has, which is a path the application made and
          -- not one of ours, so it is matched the way the platform would.
          isCurrent = maybe False (equalFilePath (rowPath r)) (scCurrent sc)
          -- The selection keeps a colour of its own when the keyboard is
          -- elsewhere, half way to the background: the row the pointer is
          -- over is drawn too, and the two have to be told apart.
          backdrop
            | selected =
                [ FillRect (Rect x ry w lineH) $
                    if scFocused sc
                      then themeSelection theme
                      else lerpColor (themeWindow theme) (themeSelection theme) 0.55
                ]
            | scHovered sc == i = [FillRect (Rect x ry w lineH) (styleHoverBg surface)]
            | otherwise = []
          -- A folder has a chevron pointing along or down; a file has none.
          chevron
            | not (rowDir r) = []
            | otherwise =
                let cx = x + indent + treeChevron / 2
                    cy = ry + lineH / 2
                 in [ if rowOpen r
                        then FillTriangle (cx - 4) (cy - 2) (cx + 4) (cy - 2) cx (cy + 3) (themeMuted theme)
                        else FillTriangle (cx - 2) (cy - 4) (cx - 2) (cy + 4) (cx + 3) cy (themeMuted theme)
                    ]
          tx = x + indent + treeChevron
          avail = w - (tx - x) - treePad - (if fromIntegral count > scViewRows sc then treeBarW else 0)
          color
            | isCurrent = themeAccent theme
            | otherwise = styleFg surface
       in backdrop ++ chevron ++ [DrawTextStyled tx (textY ry) font (elide fm avail (rowName r)) color]

    bar
      | fromIntegral count <= scViewRows sc = []
      | otherwise =
          let (thumbTop, thumbH) = thumbSpan rect count (scViewRows sc) (scScroll sc)
           in [ FillRoundedRect
                  (Rect (x + w - treeBarW + 2) (y + thumbTop + 2) (treeBarW - 4) (thumbH - 4))
                  3
                  (if scThumbHot sc then scrollBarThumbColor surface theme else styleHoverBg surface)
              ]

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
