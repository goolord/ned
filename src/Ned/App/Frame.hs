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

import Control.Monad (forM_, unless, when)
import Data.Foldable (for_)
import Data.IORef (IORef)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl
import qualified NanoUI.Shortcut as K
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
-- have one; then open whatever was dropped on the window, each in a tab, as a
-- file picked in a dialog is.
pollDialogs :: IORef App -> App -> NanoUI ()
pollDialogs ref app = do
  inp <- askInput
  let pollDialog did forget onPick =
        pollFileDialogUi did >>= \case
          FileDialogPending -> pure ()
          FileDialogSelected paths -> modifyApp ref forget >> for_ (listToMaybe paths) onPick
          _ -> modifyApp ref forget
  for_ (appOpenDlg app) $ \did -> pollDialog did (\a -> a {appOpenDlg = Nothing}) (openFile ref Nothing)
  for_ (appSaveDlg app) $ \(did, key) -> pollDialog did (\a -> a {appSaveDlg = Nothing}) (savePicked ref key)

  for_ [T.unpack (dropEventData d) | d <- foldr (:) [] (inputDrops inp), dropEventType d == DropFile] $
    openFile ref Nothing

-- | The chords the application owns, as against the ones the text owns, which
-- are the editor's in "Ned.Editor.Keys". None of them are read while the
-- question about unsaved changes is up.
--
-- Each is taken through 'shortcut', which takes the press with it, so a menu
-- row showing the same chord further down the frame does not act on it again.
appChords :: IORef App -> App -> NanoUI ()
appChords ref app = do
  inp <- askInput
  -- A frame with no key down, which is most of them, has no chord in it.
  unless (modalUp app || null (inputKeys inp)) $
    forM_ bindings $ \(chord, action) ->
      -- While the completion menu is open, Ctrl+N and Ctrl+P step through
      -- it, as they do in vim, and open no file.
      unless (isJust (edCompletion (appEditor app)) && chord `elem` [chordNew, chordFindFile]) $
        whenM (shortcut chord) action
  when (inputKeysElem KeyEscape (inputKeys inp) && appBar app /= BarNone && not (modalUp app)) (closeBar ref)
  where
    bindings
      | isJust (edVim (appEditor app)) = filter ((`notElem` vimChords) . fst) (appBindings ref) <> vimBindings ref
      | otherwise = appBindings ref

-- | Every chord of the application's and what it does: the ones the menus
-- show, and the others a keyboard reaches for as well.
appBindings :: IORef App -> [(K.Shortcut, NanoUI ())]
appBindings ref =
  [ (chordSave, save ref False)
  , (chordSaveAs, save ref True)
  , (chordOpen, openDialog ref)
  , (chordNew, newFile ref)
  , (chordQuit, guarded ref PendingQuit)
  , (chordCloseTab, readApp ref >>= closeTab ref . appDocKey)
  , (chordNextTab, stepTab ref True)
  , (chordPrevTab, stepTab ref False)
  , (K.ctrl <> K.key KeyPageDown, stepTab ref True)
  , (K.ctrl <> K.key KeyPageUp, stepTab ref False)
  , (chordGrep, openPicker ref grepSource)
  , (chordFind, openBar ref BarFind)
  , (chordGoto, openBar ref BarGoto)
  , (chordTree, toggleTree ref)
  , (chordFindFile, openPicker ref fileSource)
  , (chordDefinition, gotoDefinition ref)
  , (chordZoomIn, zoom ref (* 1.1))
  , -- Ctrl and the key that has + on it, with and without the Shift that
    -- types the +, and the keypad's.
    (K.ctrl <> K.shift <> K.key '=', zoom ref (* 1.1))
  , (K.ctrl <> K.key '+', zoom ref (* 1.1))
  , (chordZoomOut, zoom ref (/ 1.1))
  , (chordZoomReset, resetZoom ref)
  ]

-- | With vim's keys, Ctrl and a direction moves the keyboard between the
-- tree, the text and the bar, as Ctrl+W and a direction does in vim.
vimBindings :: IORef App -> [(K.Shortcut, NanoUI ())]
vimBindings ref = [(K.ctrl <> K.key c, focusToward ref c) | c <- "hjkl"]

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
chromeSig :: App -> (Text, Bool, Bool, Bool, Text, (Bool, Bool, Int), (Int, [(Int, Bool)], (Int, Maybe FilePath)))
chromeSig a =
  ( appOpenMenu a
  , appBar a == BarNone
  , appBarFocus a
  , isJust (appPending a)
  , appStatus a
  , (appTreeShown a, appTreeFocus a, FT.ftVersion (appTree a))
  , ( -- The finder gathers on a thread of its own, so it is the one part of
      -- the window that changes between frames without anybody having
      -- touched a key.
      pickerSig (appPicker a)
    , -- The tabs, which are drawn above the text that makes one of them
      -- dirty. Only the one in front is renamed, by saving it under a name.
      [(docKey d, docDirty d) | d <- appDocs a]
    , (appDocKey a, appPath a)
    )
  )

-- | What of the application the editor draws.
editorSig :: App -> (Int, Int, Int, (B.Matching, Text), (Bool, Bool), Float, Text)
editorSig a =
  ( B.bufVersion buf
  , B.bufCursor buf
  , B.bufAnchor buf
  , findMarks a
  , (edReveal ed, edShowWhitespace ed)
  , edFontSize ed
  , langName (edLang ed)
  )
  where
    ed = appEditor a
    buf = edBuffer ed
