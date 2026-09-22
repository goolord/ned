-- | What one frame of the application does besides draw: the file dialogs it
-- asks for an answer, the files dropped on the window, the chords it answers
-- to, the window's title, and whether the frame after it has to draw at all.
--
-- None of this places anything. Every one of these is a function of the state
-- and the frame's input, so the frame in "Ned.View" is left saying when each
-- is asked for, in what order, and what goes where.
module Ned.App.Frame
  ( -- * What a frame answers to
    pollDialogs
  , appChords
  , syncTitle

    -- * Whether the next frame draws
  , chromeSig
  , editorSig
  ) where

import Control.Monad (forM_, when)
import Data.Foldable (for_)
import Data.IORef (IORef)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
import Ned.App.Commands
import Ned.App.State
import qualified Ned.Buffer as B
import Ned.Editor
import qualified Ned.FileTree as FT
import Ned.Highlight (langName)
import Ned.Picker (fileSource, grepSource, pickerSig)

--------------------------------------------------------------------------------
-- What a frame answers to
--------------------------------------------------------------------------------

-- | Ask the dialogs that are up for their answer, and put away the ones that
-- have one; then open whatever was dropped on the window, which opens as a
-- file picked in a dialog does.
pollDialogs :: IORef App -> App -> NanoUI ()
pollDialogs ref app = do
  inp <- askInput
  let pollDialog dialog forget onPick =
        for_ (dialog app) $ \did ->
          pollFileDialogUi did >>= \case
            FileDialogPending -> pure ()
            FileDialogSelected paths -> modifyApp ref forget >> for_ (listToMaybe paths) onPick
            _ -> modifyApp ref forget
  pollDialog appOpenDlg (\a -> a {appOpenDlg = Nothing}) (runPending ref . PendingOpenPath Nothing)
  pollDialog appSaveDlg (\a -> a {appSaveDlg = Nothing}) (saveTo ref)

  for_ [T.unpack (dropEventData d) | d <- foldr (:) [] (inputDrops inp), dropEventType d == DropFile] $
    guarded ref . PendingOpenPath Nothing

-- | The chords the application owns, as against the ones the text owns, which
-- are the editor's in "Ned.Editor.Keys". None of them are read while the
-- question about unsaved changes is up.
appChords :: IORef App -> App -> NanoUI ()
appChords ref app = do
  inp <- askInput
  let mods = inputModifiers inp
  when (modCtrl mods && not (modAlt mods) && not (modalUp app)) $
    forM_ (T.unpack (inputChars inp)) $ \case
      's' | modShift mods -> save ref True
      's' -> save ref False
      'S' -> save ref True
      'o' -> guarded ref PendingOpen
      'n' -> guarded ref PendingNew
      'q' -> guarded ref PendingQuit
      'f' | modShift mods -> openPicker ref grepSource
      'F' -> openPicker ref grepSource
      'f' -> openBar ref BarFind
      'g' -> openBar ref BarGoto
      'b' -> toggleTree ref
      'p' -> openPicker ref fileSource
      '=' -> zoom ref (* 1.1)
      '+' -> zoom ref (* 1.1)
      '-' -> zoom ref (/ 1.1)
      '0' -> zoom ref (const defaultFontSize)
      _ -> pure ()
  when (inputKeysElem KeyEscape (inputKeys inp) && appBar app /= BarNone && not (modalUp app)) (closeBar ref)

-- | Put the file's name on the window when it is not there already. The title
-- as last set is kept in the state, so this is one comparison a frame and a
-- call to the host only when the answer changed.
syncTitle :: IORef App -> App -> NanoUI ()
syncTitle ref app =
  when (titleFor app /= appTitle app) $ do
    modifyApp ref (\a -> a {appTitle = titleFor app})
    setWindowTitleUi (titleFor app)

--------------------------------------------------------------------------------
-- Whether the next frame draws
--------------------------------------------------------------------------------

-- | What of the application the chrome draws. The state is in an 'IORef' that
-- nano-ui knows nothing of, so a frame that changed it after the part showing
-- it was declared (a menu button opening its menu, a menu row editing the
-- text, the find field setting what is marked) has to ask for the frame that
-- shows it.
--
-- The tree's width is not in here: the pane grid marks its own damage while
-- one of its bars is dragged.
chromeSig :: App -> (Text, Bool, Bool, Bool, Text, (Bool, Bool, Int), Int)
chromeSig a =
  ( appOpenMenu a
  , appBar a == BarNone
  , appBarFocus a
  , isJust (appPending a)
  , appStatus a
  , (appTreeShown a, appTreeFocus a, FT.ftVersion (appTree a))
  , -- The finder gathers on a thread of its own, so it is the one part of the
    -- window that changes between frames without anybody having touched a key.
    pickerSig (appPicker a)
  )

-- | What of the application the editor draws.
editorSig :: App -> (Int, Int, Int, Text, (Bool, Bool, Bool), Float, Text)
editorSig a =
  ( B.bufVersion buf
  , B.bufCursor buf
  , B.bufAnchor buf
  , findMarks a
  , (edFindExact ed, edReveal ed, edShowWhitespace ed)
  , edFontSize ed
  , langName (edLang ed)
  )
  where
    ed = appEditor a
    buf = edBuffer ed
