-- | The entry point: a window, the state one frame runs on, and the frames.
--
-- Everything the window is made of is elsewhere -- what it draws in
-- "Ned.View", what it is between frames in "Ned.App.State", what it can be
-- asked to do in "Ned.App.Commands", what a frame answers to in
-- "Ned.App.Frame" -- so what is left here is starting it: the settings, the
-- files named on the command line, the window's size and theme, and the two
-- environment variables that time a frame.
module Ned.App
  ( -- * Running
    runNed

    -- * The state it runs on
  , App (..)
  , newApp
  , Placement (..)
  , openPath
  ) where

import Control.Monad (foldM)
import Data.IORef (newIORef)
import qualified Data.Text as T
import NanoUI (Size (..), tomorrowNightMinDarkTheme)
import NanoUI.Backend.Sdl
import Ned.App.Commands (fontFor)
import Ned.App.State
import Ned.Config
import Ned.View (appView, blankView, tracedView)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

-- | Run the editor, on the files given, a tab each.
runNed :: [FilePath] -> IO ()
runNed paths = do
  path <- configPath
  (cfg, problem) <- either (\e -> (defaultConfig, Just e)) id <$> readConfig path
  -- What was wrong with the settings goes to the terminal in full, and the
  -- status bar says where to look, over whatever opening the files said.
  mapM_ (hPutStrLn stderr) problem
  fresh <- newApp cfg
  opened <- foldM (\app file -> openPath InNewTab Nothing file app) fresh paths
  let app0 =
        (maybe opened (const opened {appStatus = "Some settings in " <> T.pack path <> " were not used; see the terminal"}) problem)
          { appConfigPath = Just path
          }
  ref <- newIORef app0
  -- NED_TRACE names a file to log a line a frame to: the time, the window's
  -- size, and what the frame cost.
  trace <- lookupEnv "NED_TRACE"
  -- NED_BLANK swaps the application for one label, to tell what a frame
  -- costs the toolkit from what it costs the editor.
  blank <- lookupEnv "NED_BLANK"
  let body = maybe (appView ref) (const blankView) blank
      view = maybe body (`tracedView` body) trace
  runSdlApp
    defaultSdlOptions
      { sdlWindowSettings =
          defaultWindowSettings
            { wsTitle = titleFor app0
            , wsSize = Size (fromIntegral (cfgWindowWidth cfg)) (fromIntegral (cfgWindowHeight cfg))
            }
      , -- The window has no title bar of the desktop's: its title, its
        -- buttons and the strip that drags it are all in the bar along the
        -- top of the frame, in "Ned.View.Chrome". It keeps the desktop's
        -- frame, which is what still resizes it and carries its shadow.
        sdlWindowDecorations = DecorationsFrame
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      , sdlAppFontSize = cfgUiFontSize cfg
      , sdlAppUiScale = cfgScale cfg
      , sdlAppFont = fontFor (sdlAppFont defaultSdlOptions) (cfgUiFont cfg)
      , sdlAppMonoFont = fontFor (sdlAppMonoFont defaultSdlOptions) (cfgBufferFont cfg)
      }
    view
