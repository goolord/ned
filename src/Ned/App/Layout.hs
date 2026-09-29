-- | The window as it was left, kept from one run to the next: its size,
-- whether it filled the screen, and the tree's visibility, width and position.
--
-- It is kept apart from the settings, in ned's folder of the user's state
-- (@~/.local/state/ned@ on Linux, @%LOCALAPPDATA%\\ned@ on Windows), as JSON
-- that nobody is meant to write. The settings are what a first run opens
-- with; after that the window opens as it was closed. A field the file does
-- not have, or cannot be read, keeps what the settings say.
module Ned.App.Layout
  ( layoutPath
  , loadLayout
  , saveLayout
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Aeson.Micro (Object, Parser, Value, decodeStrict, encodeStrict, object, parseMaybe, withObject, (.!=), (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import NanoUI (Size (..))
import qualified NanoUI as G (GridAxis (..), GridNode (..))
import Ned.App.State
import Ned.FileTree (minTreeWidth)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

-- | Where the layout is kept.
layoutPath :: IO FilePath
layoutPath = (</> "layout.json") <$> getXdgDirectory XdgState "ned"

-- | The application with the layout it was last closed with, over what it
-- has from the settings. No file, or one that does not read, leaves it as
-- it is.
loadLayout :: FilePath -> App -> IO App
loadLayout path app =
  try @IOException (BS.readFile path) >>= \case
    Left _ -> pure app
    Right bytes -> pure (fromMaybe app (decodeStrict bytes >>= parseMaybe (withObject "layout" (laidOver app))))

-- | The fields the file has, each over what the application has already.
-- A size is kept to what is still a window, and the tree to what it can be
-- dragged to.
laidOver :: App -> Object -> Parser App
laidOver app o = do
  let WindowLayout (Size w h) maximized treeW outerGrid = appLayout app
  w' <- o .:? "width" .!= w
  h' <- o .:? "height" .!= h
  maximized' <- o .:? "maximized" .!= maximized
  treeW' <- o .:? "treeWidth" .!= treeW
  shown <- o .:? "treeShown" .!= appTreeShown app
  outerGridValue <- o .:? "outerGrid"
  let outerGrid' = case outerGridValue >>= parseMaybe parseGridNode of
        Just tree | validOuterGrid tree -> Just tree
        _ -> outerGrid
  pure
    app
      { appLayout = WindowLayout (Size (max 320 w') (max 240 h')) maximized' (max minTreeWidth treeW') outerGrid'
      , appTreeShown = shown
      }

-- | Read the small JSON tree that describes the outer pane arrangement.
parseGridNode :: Value -> Parser G.GridNode
parseGridNode = withObject "pane grid node" $ \node -> do
  pane <- node .:? "pane"
  case pane of
    Just paneId -> pure (G.Pane paneId)
    Nothing -> do
      splitId <- node .: "split"
      axisName <- (node .: "axis" :: Parser T.Text)
      axis <- case axisName of
        "vertical" -> pure G.AxisV
        "horizontal" -> pure G.AxisH
        _ -> fail "unknown pane grid axis"
      ratio <- node .: "ratio"
      first <- node .: "first" >>= parseGridNode
      second <- node .: "second" >>= parseGridNode
      pure (G.Split splitId axis ratio first second)

-- | The outer grid always holds these two panes. Reject a stale or malformed
-- tree rather than restoring an arrangement the views cannot fill.
validOuterGrid :: G.GridNode -> Bool
validOuterGrid (G.Split splitId _ ratio (G.Pane first) (G.Pane second)) =
  ((first == treePaneId && second == editorPaneId) || (first == editorPaneId && second == treePaneId))
    && splitId > 0
    && splitId < (2 ^ (63 :: Int))
    && splitId /= first
    && splitId /= second
    && not (isNaN ratio || isInfinite ratio)
    && ratio >= 0
    && ratio <= 1
validOuterGrid _ = False

-- | Keep the application's layout for the next run. The file is written
-- beside itself and moved over the old one, so a run that is cut short
-- leaves the old layout rather than half of a new one. A layout that cannot
-- be written is said so on the terminal and is otherwise no matter.
saveLayout :: FilePath -> App -> IO ()
saveLayout path app = do
  let tmp = path <> ".new"
  written <- try @IOException $ do
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile tmp (encodeStrict (layoutJson app))
    renameFile tmp path
  either (\e -> hPutStrLn stderr ("The layout was not saved: " <> show e)) pure written

layoutJson :: App -> Value
layoutJson app =
  object
    [ "width" .= w
    , "height" .= h
    , "maximized" .= maximized
    , -- Whole, which is what it was dragged to; what was drawn is a share
      -- of the row, and a hair off.
      "treeWidth" .= (fromIntegral (round treeW :: Int) :: Float)
    , "treeShown" .= appTreeShown app
    , "outerGrid" .= (gridNodeValue <$> layoutOuterGrid (appLayout app))
    ]
  where
    WindowLayout (Size w h) maximized treeW _ = appLayout app

gridNodeValue :: G.GridNode -> Value
gridNodeValue (G.Pane paneId) = object ["pane" .= paneId]
gridNodeValue (G.Split splitId axis ratio first second) =
  object
    [ "split" .= splitId
    , "axis" .= axisName
    , "ratio" .= ratio
    , "first" .= gridNodeValue first
    , "second" .= gridNodeValue second
    ]
  where
    axisName :: T.Text
    axisName = case axis of
      G.AxisV -> "vertical"
      G.AxisH -> "horizontal"
