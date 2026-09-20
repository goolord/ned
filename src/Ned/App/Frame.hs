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
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Monad (askHost, askInput)
import Ned.App.Commands
import Ned.App.State
import qualified Ned.Buffer as B
import Ned.Editor
import qualified Ned.FileTree as FT
import Ned.Highlight (langName)
import Ned.Sdl (setWindowTitle)

--------------------------------------------------------------------------------
-- What a frame answers to
--------------------------------------------------------------------------------

-- | Ask the dialogs that are up for their answer, and put away the ones that
-- have one; then open whatever was dropped on the window, which opens as a
-- file picked in a dialog does.
pollDialogs :: Commands -> App -> NanoUI ()
pollDialogs cmds app = do
  inp <- askInput
  let modify = cmdModify cmds
      pollDialog dialog forget onPick =
        for_ (dialog app) $ \did ->
          pollFileDialogUi did >>= \case
            FileDialogPending -> pure ()
            FileDialogSelected paths -> modify forget >> for_ (listToMaybe paths) onPick
            _ -> modify forget
  pollDialog appOpenDlg (\a -> a {appOpenDlg = Nothing}) (cmdRun cmds . PendingOpenPath)
  pollDialog appSaveDlg (\a -> a {appSaveDlg = Nothing}) (cmdSaveTo cmds)

  for_ [T.unpack (dropEventData d) | d <- foldr (:) [] (inputDrops inp), dropEventType d == DropFile] $
    cmdGuarded cmds . PendingOpenPath

-- | The chords the application owns, as against the ones the text owns, which
-- are the editor's in "Ned.Editor.Keys". None of them are read while the
-- question about unsaved changes is up.
appChords :: Commands -> App -> NanoUI ()
appChords cmds app = do
  inp <- askInput
  let mods = inputModifiers inp
      blocked = isJust (appPending app)
  when (modCtrl mods && not (modAlt mods) && not blocked) $
    forM_ (T.unpack (inputChars inp)) $ \case
      's' | modShift mods -> cmdSave cmds True
      's' -> cmdSave cmds False
      'S' -> cmdSave cmds True
      'o' -> cmdGuarded cmds PendingOpen
      'n' -> cmdGuarded cmds PendingNew
      'q' -> cmdGuarded cmds PendingQuit
      'f' -> cmdOpenBar cmds BarFind
      'g' -> cmdOpenBar cmds BarGoto
      'b' -> cmdToggleTree cmds
      '=' -> cmdZoom cmds (* 1.1)
      '+' -> cmdZoom cmds (* 1.1)
      '-' -> cmdZoom cmds (/ 1.1)
      '0' -> cmdZoom cmds (const defaultFontSize)
      _ -> pure ()
  when (inputKeysElem KeyEscape (inputKeys inp) && appBar app /= BarNone && not blocked) (cmdCloseBar cmds)

-- | Put the file's name on the window when it is not there already. The title
-- as last set is kept in the state, so this is one comparison a frame and a
-- call to the host only when the answer changed.
syncTitle :: Commands -> App -> NanoUI ()
syncTitle cmds app =
  when (titleFor app /= appTitle app) $ do
    cmdModify cmds (\a -> a {appTitle = titleFor app})
    host <- askHost
    for_ host $ \env -> uiIO (setWindowTitle env (titleFor app))

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
chromeSig :: App -> (Text, Bool, Bool, Bool, Text, (Bool, Bool, Int))
chromeSig a =
  ( appOpenMenu a
  , appBar a == BarNone
  , appBarFocus a
  , isJust (appPending a)
  , appStatus a
  , (appTreeShown a, appTreeFocus a, FT.ftVersion (appTree a))
  )

-- | What of the application the editor draws.
editorSig :: App -> (Int, Int, Int, Text, (Bool, Bool, Bool), Float, Text)
editorSig a =
  ( B.bufVersion buf
  , B.bufCursor buf
  , B.bufAnchor buf
  , edFind ed
  , (edFindExact ed, edReveal ed, edShowWhitespace ed)
  , edFontSize ed
  , langName (edLang ed)
  )
  where
    ed = appEditor a
    buf = edBuffer ed
