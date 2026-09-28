-- | The window as it was left, kept from one run to the next: its size,
-- whether it filled the screen, and whether the tree was shown and how wide.
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
import Data.Aeson.Micro (Object, Parser, Value, decodeStrict, encodeStrict, object, parseMaybe, withObject, (.!=), (.:?), (.=))
import Data.Maybe (fromMaybe)
import NanoUI (Size (..))
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
  let WindowLayout (Size w h) maximized treeW = appLayout app
  w' <- o .:? "width" .!= w
  h' <- o .:? "height" .!= h
  maximized' <- o .:? "maximized" .!= maximized
  treeW' <- o .:? "treeWidth" .!= treeW
  shown <- o .:? "treeShown" .!= appTreeShown app
  pure
    app
      { appLayout = WindowLayout (Size (max 320 w') (max 240 h')) maximized' (max minTreeWidth treeW')
      , appTreeShown = shown
      }

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
    ]
  where
    WindowLayout (Size w h) maximized treeW = appLayout app
