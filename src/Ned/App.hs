-- | The entry point: a window, the state one frame runs on, and the frames.
--
-- Everything the window is made of is elsewhere -- what it draws in
-- "Ned.View", what it is between frames in "Ned.App.State", what it can be
-- asked to do in "Ned.App.Commands", what a frame answers to in
-- "Ned.App.Frame" -- so what is left here is starting it: the file named on
-- the command line, the window's size and theme, and the two environment
-- variables that time a frame.
module Ned.App
  ( -- * Running
    runNed

    -- * The state it runs on
  , App (..)
  , newApp
  , newAppIn
  , openPath
  ) where

import Data.IORef (newIORef)
import NanoUI (Size (..), tomorrowNightMinDarkTheme)
import NanoUI.Backend.Sdl
import Ned.App.State
import Ned.View (appView, blankView, tracedView)
import System.Environment (lookupEnv)

-- | Run the editor, on a file if one is given.
runNed :: Maybe FilePath -> IO ()
runNed mpath = do
  blankApp <- newAppIn
  app0 <- maybe (pure blankApp) (`openPath` blankApp) mpath
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
      { sdlWindowTitle = titleFor app0
      , sdlWindowSize = Size 1100 760
      , -- The window has no title bar of the desktop's: its title, its
        -- buttons and the strip that drags it are all in the bar along the
        -- top of the frame, in "Ned.View.Chrome". It keeps the desktop's
        -- frame, which is what still resizes it and carries its shadow.
        sdlWindowDecorations = DecorationsFrame
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      }
    view
