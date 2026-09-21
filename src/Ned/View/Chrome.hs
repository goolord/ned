-- | The bars around the editor: the window's own title bar along the top,
-- the find and go-to-line bar under the text, and the status along the
-- bottom.
--
-- None of this edits anything itself. Every row and button asks for a
-- 'Commands', so what a menu says and what it does sit on the same line, and
-- the frame in "Ned.View" is left to say where the bars go.
module Ned.View.Chrome
  ( -- * The window's own chrome
    titleBar
  , windowBorder
  , appMenus
  , editorMenu
  , treeMenu

    -- * The bars
  , editorBar
  , statusBar
  ) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import NanoUI.Backend.Sdl (CaptionOptions (..), defaultCaptionOptions, defaultResizeBorder, windowCaptionWith, windowMaximizedUi)
import NanoUI.Context (Context (..), getFocusId)
import NanoUI.Monad (askContext, askFrameInput, askInput)
import Ned.App.Commands
import Ned.App.State
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Editor
import Ned.File (Eol (..), FileFormat (..))
import Ned.FileTree (hasParentRoot)
import qualified Ned.FileTree as FT
import Ned.Highlight (langName)
import qualified Ned.Picker as P
import Ned.Theme (closeRed, menuChrome, windowEdge)
import System.FilePath (takeFileName)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
-- The title bar
--------------------------------------------------------------------------------

-- | How tall the bar along the top is: the height the toolkit's own caption
-- buttons are drawn at, since they sit in it.
titleBarHeight :: Float
titleBarHeight = captionBarHeight

-- | How thick the line around the whole window is. One pixel: it is there
-- to say where the window ends, not to be seen. What colour it is is
-- "Ned.Theme"'s 'windowEdge'.
--
-- It is drawn square, because the window is. The window the desktop rounds
-- is the one the frame is the edge of, a frame's width outside the view; the
-- view's own corners are square whatever is done there, and the toolkit
-- squares the shadow to match rather than leave it curving round corners the
-- window does not have.
windowBorderWidth :: Float
windowBorderWidth = 1

-- | The line around the whole window, with the frame inside it. The window
-- keeps none of the desktop's title bar and nothing of its frame is drawn,
-- so this line is all there is to tell the window from what is behind it on
-- the desktop.
--
-- A window filling the screen draws none: what is beside it is the screen's
-- own edge, and a line there would be a line against nothing. The container
-- stays either way, so nothing inside loses its place when the line comes
-- and goes.
windowBorder :: NanoUI a -> NanoUI a
windowBorder body = do
  theme <- uiTheme
  maximized <- windowMaximizedUi
  windowFrame
    WindowFrame
      { frameWidth = if maximized then 0 else windowBorderWidth
      , frameRadius = 0
      , frameColor = windowEdge theme
      }
    body

-- | The window's title bar, which is also its menu bar: the menus at the
-- left, the file's name in the middle, and the buttons that put the window
-- away, fill the screen with it and close it at the right.
--
-- The desktop's own title bar is gone, so this row is the only thing there
-- is to take hold of the window by. What is left of it between the menus and
-- those buttons is handed over as the strip that drags the window, which is
-- 'windowCaptionWith''s to do: it wants the rectangles of everything in the
-- row that takes a click of its own, and the menu buttons hand theirs back
-- as they are drawn.
titleBar :: Commands -> App -> NanoUI ()
titleBar cmds app = do
  closing <-
    styled menuChrome $
      rowWith (titleBarPad . tight . fillW . fixedH titleBarHeight . gap 2) $ do
        taken <- menus cmds app
        flex
        -- The same words the desktop has for the window, since this bar is
        -- now the only place they are written.
        labelWith (tight . fontMuted . alignMid) (titleFor app)
        flex
        -- The window's three buttons sit against each other and against the
        -- end of the bar, as a window's own do, so they are in a row of
        -- their own that the bar's gap does not reach into.
        rowWith (tight . gap 0 . fillH) $
          -- The sides and the bottom resize the window from outside it,
          -- where the desktop's own frame is, so inside them the line the
          -- window draws round itself is as far in as an edge reaches. The
          -- top has no frame outside it and never can have -- that strip
          -- would be painted as a caption -- so the top of the bar is what
          -- the top edge is grasped by, and it takes the room it needs.
          windowCaptionWith
            defaultCaptionOptions
              { capResizeBorder = windowBorderWidth
              , capResizeTop = defaultResizeBorder
              , -- The close button reaches a corner of a window with square
                -- corners, so its own is square too.
                capButtons = defaultCaptionConfig {capCornerRadius = 0, capCloseColor = Just closeRed}
              }
            taken
  -- Closing throws away the same unsaved text File > Exit does, so it asks
  -- the same question first.
  when closing (cmdGuarded cmds PendingQuit)

-- | The bar's padding. The menu titles start in from the edge rather than
-- hard against it; the window's own buttons at the other end do finish hard
-- against it, so that the close button reaches the corner the way every
-- other window's does.
titleBarPad :: Layout -> Layout
titleBarPad l = l {layoutPadding = Padding 6 0 0 0}

-- | The menu bar's buttons and the menus that hang from them, and where each
-- button is, which is where the window cannot be dragged by.
--
-- A popup is dismissed by the press of a click outside it, and a button is
-- clicked by the release. A click on the open menu's own button is both: the
-- press closes the menu, and the release would open it again. So a press
-- that closes a menu over its own button is remembered in the state, and the
-- click that follows it does nothing.
menus :: Commands -> App -> NanoUI [Rect]
menus cmds app = do
  -- The pointer as it is, where a button under an open menu sees none.
  pointer <- askFrameInput
  rects <- traverse (entry pointer) (appMenus cmds app)
  when (inputMouseReleased pointer && not (T.null swallow)) (setSwallow "")
  pure rects
  where
    open = appOpenMenu app
    swallow = appMenuSwallow app
    setOpen m = cmdModify cmds (\a -> a {appOpenMenu = m})
    setSwallow m = cmdModify cmds (\a -> a {appMenuSwallow = m})
    entry pointer (title, body) = do
      let isOpen = open == title
      btn <- menuButtonWith' fillH title isOpen
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
          onButton = rectContains (respRect btn) (inputMousePos pointer)
      when (respClicked btn && swallow /= title) (setOpen (if isOpen then "" else title))
      when (not isOpen && not (T.null open) && respHovered btn) (setOpen title)
      (popupResp, _) <- popup isOpen cfg (columnWith (tight . gap 0) body)
      when (respClicked popupResp) $ do
        setOpen ""
        when (inputMousePressed pointer && onButton) (setSwallow title)
      pure (respRect btn)

-- | A row of a menu, greyed when it does not apply. Picking one closes the
-- menu bar's menu, which a context menu has none of and loses nothing by.
menuEntry :: Commands -> Bool -> Text -> Text -> NanoUI () -> NanoUI ()
menuEntry cmds ok lbl shortcut action
  | not ok = menuItemDisabled lbl
  | otherwise = whenM (if T.null shortcut then menuItem lbl else menuItemShortcut lbl shortcut) (cmdCloseMenu cmds >> action)

-- | The menus along the top, and what each of their rows does. A row that
-- turns something on and off says what it would do rather than what is so:
-- the menu is read to change something, not to find out how it stands.
appMenus :: Commands -> App -> [(Text, NanoUI ())]
appMenus cmds app = [("File", fileMenu), ("Edit", editMenu), ("View", viewMenu)]
  where
    entry = menuEntry cmds
    item = entry True
    buf0 = edBuffer (appEditor app)
    fileMenu = do
      item "New" "Ctrl+N" (cmdGuarded cmds PendingNew)
      item "Open..." "Ctrl+O" (cmdGuarded cmds PendingOpen)
      item "Find File..." "Ctrl+P" (cmdOpenPicker cmds P.fileSource)
      item "Save" "Ctrl+S" (cmdSave cmds False)
      item "Save As..." "Ctrl+Shift+S" (cmdSave cmds True)
      menuSeparator
      item "Exit" "Ctrl+Q" (cmdGuarded cmds PendingQuit)
    editMenu = do
      editEntries cmds buf0
      menuSeparator
      item "Find..." "Ctrl+F" (cmdOpenBar cmds BarFind)
      item "Search in Files..." "Ctrl+Shift+F" (cmdOpenPicker cmds P.grepSource)
      item "Go to Line..." "Ctrl+G" (cmdOpenBar cmds BarGoto)
    viewMenu = do
      item
        (if appTreeShown app then "Hide File Tree" else "Show File Tree")
        "Ctrl+B"
        (cmdToggleTree cmds)
      menuSeparator
      item "Zoom In" "Ctrl+=" (cmdZoom cmds (* 1.1))
      item "Zoom Out" "Ctrl+-" (cmdZoom cmds (/ 1.1))
      item "Reset Zoom" "Ctrl+0" (cmdZoom cmds (const defaultFontSize))
      menuSeparator
      item
        (if edShowWhitespace (appEditor app) then "Hide Indentation Marks" else "Show Indentation Marks")
        ""
        (cmdOnEditor cmds (\e -> e {edShowWhitespace = not (edShowWhitespace e)}))
      item
        (if B.usesTabs buf0 then "Indent with Spaces" else "Indent with Tabs")
        ""
        (cmdOnBuffer cmds (B.setUsesTabs (not (B.usesTabs buf0))))
      item
        (if formatEol (appFormat app) == LF then "Line Endings: CRLF" else "Line Endings: LF")
        ""
        (cmdModify cmds (\a -> a {appFormat = (appFormat a) {formatEol = if formatEol (appFormat a) == LF then CRLF else LF}}))

-- | What the Edit menu and the editor's own menu both start with.
editEntries :: Commands -> Buffer -> NanoUI ()
editEntries cmds buf = do
  entry (B.canUndo buf) "Undo" "Ctrl+Z" (cmdOnBuffer cmds B.undo)
  entry (B.canRedo buf) "Redo" "Ctrl+Y" (cmdOnBuffer cmds B.redo)
  menuSeparator
  item "Cut" "Ctrl+X" (cmdOnBufferIO cmds clipboardCut)
  item "Copy" "Ctrl+C" (cmdOnBufferIO cmds clipboardCopy)
  item "Paste" "Ctrl+V" (cmdOnBufferIO cmds clipboardPaste)
  menuSeparator
  item "Select All" "Ctrl+A" (cmdOnBuffer cmds B.selectAll)
  where
    entry = menuEntry cmds
    item = entry True

-- | The editor's own menu, on the right button.
editorMenu :: Commands -> Buffer -> NanoUI ()
editorMenu cmds buf = do
  editEntries cmds buf
  menuEntry cmds True "Find..." "Ctrl+F" (cmdOpenBar cmds BarFind)

-- | The file tree's own menu, on the right button.
treeMenu :: Commands -> App -> NanoUI ()
treeMenu cmds app = do
  entry (isJust (appPath app)) "Reveal Current File" "" (for_ (appPath app) (cmdOnTree cmds . FT.reveal))
  entry (hasParentRoot (appTree app)) "Open Parent Folder" "" (cmdOnTree cmds FT.parentRoot)
  menuSeparator
  item "Collapse All" "" (cmdOnTree cmds FT.collapseAll)
  item "Refresh" "" (cmdOnTree cmds FT.refresh)
  menuSeparator
  item "Hide File Tree" "" (cmdToggleTree cmds)
  where
    entry = menuEntry cmds
    item = entry True

--------------------------------------------------------------------------------
-- The bar under the editor
--------------------------------------------------------------------------------

-- | Find, or go to line, or nothing at all: the bar between the text and the
-- status. A press on its field takes the keyboard back from the editor or the
-- tree, and it keeps it for as long as the bar is up.
editorBar :: Commands -> App -> NanoUI ()
editorBar cmds app = case appBar app of
  BarNone -> pure ()
  BarFind ->
    barRow "Find" (appFindText app) $ \query -> do
      when (query /= appFindText app) $ do
        cmdModify cmds (\a -> a {appFindText = query, appEditor = (appEditor a) {edFind = query}})
        -- Search as the query is typed, from where the selection starts.
        cmdOnBuffer cmds (\b -> B.setCursor False (fst (B.selectionRange b)) b)
        cmdFind cmds True
      exact <- barCheckbox "Match case" (edFindExact (appEditor app))
      when (exact /= edFindExact (appEditor app)) (cmdOnEditor cmds (\e -> e {edFindExact = exact}))
      separator
      -- The buttons are subtle: a toolbar row does not want three filled
      -- grey chips, only the press and the hover to be seen.
      styled subtle $ do
        whenM (buttonWith (tight . alignMid) "Previous") (cmdFind cmds False)
        whenM (buttonWith (tight . alignMid) "Next") (cmdFind cmds True)
        whenM (buttonWith (tight . alignMid) "Close") (cmdCloseBar cmds)
      enter <- pressedEnter
      shift <- heldShift
      when enter (cmdFind cmds (not shift))
  BarGoto ->
    barRow "Go to line" (appGotoText app) $ \txt -> do
      when (txt /= appGotoText app) (cmdModify cmds (\a -> a {appGotoText = txt}))
      separator
      go <- styled subtle (buttonWith (tight . alignMid) "Go")
      whenM (styled subtle (buttonWith (tight . alignMid) "Close")) (cmdCloseBar cmds)
      enter <- pressedEnter
      when (enter || go) $
        case readMaybe (T.unpack (T.strip txt)) of
          Just n -> cmdOnBuffer cmds (B.gotoLine n) >> cmdCloseBar cmds
          Nothing -> cmdStatus cmds "Not a line number"
  where
    pressedEnter = do
      inp <- askInput
      pure (inputKeysElem KeyEnter (inputKeys inp) && appBarFocus app)
    heldShift = modShift . inputModifiers <$> askInput
    -- Keep the keyboard in the bar's field while the bar has it.
    holdFocus resp = when (appBarFocus app) $ do
      ctx <- askContext
      uiIO $ do
        focus <- getFocusId ctx
        when (focus /= respId resp) $ writeIORef (ctxFocusId ctx) (respId resp)
    -- A bar: its name, its field, and whatever comes after the field. The
    -- name is set at full strength and semibold, the way the tree's header
    -- and the status bar's file are, so the bar reads as naming what it is
    -- for rather than as one muted place more among the grey.
    barRow name value rest = do
      separator
      rowWith (padXY 12 5 . tight . fillW . gap 8 . alignMid) $ do
        labelWith (tight . fontSemiBold . alignMid) name
        (resp, txt) <- textInput' value
        holdFocus resp
        when (respPressed resp) (cmdModify cmds (\a -> a {appBarFocus = True}))
        rest txt

-- | A checkbox on the bar's middle line. The toolkit's own 'checkbox' takes no
-- layout, so it cannot be told to centre itself in a row whose height is set by
-- the taller find field; a column that fills the row and pads equally above and
-- below puts it where the row's 'alignMid' means it to go.
barCheckbox :: Text -> Bool -> NanoUI Bool
barCheckbox caption checked =
  columnWith (tight . gap 0 . fillH) $ do
    spacer Fit (Grow 1)
    on <- checkbox caption checked
    spacer Fit (Grow 1)
    pure on

--------------------------------------------------------------------------------
-- The status bar
--------------------------------------------------------------------------------

-- | The bar along the bottom. What is on it is in three voices rather than
-- one: the file it is about is set semibold, the place the caret is in it is
-- set at full strength because it changes as you type, and the facts that
-- only sit there are muted. A row of eight labels all in the same grey is a
-- row nobody reads.
--
-- A setting that is at its default says nothing at all. Every file is at 100%
-- zoom and most have no byte order mark, so showing either of those on every
-- file costs a place on the bar and tells you what you already assumed; they
-- appear when they are worth a look and are quiet the rest of the time.
statusBar :: App -> NanoUI ()
statusBar app =
  rowWith (padXY 12 5 . tight . gap 12 . fillW) $ do
    labelWith (tight . fontMuted) (appStatus app)
    flex
    -- A file with changes to save carries a dot, the way an editor's tab
    -- does. An asterisk read as part of the name.
    labelWith (tight . fontSemiBold) (name <> (if B.isDirty buf then " \x2022" else ""))
    labelWith tight ("Ln " <> showT (ln + 1) <> ", Col " <> showT (col + 1))
    labelWith (tight . fontMuted) (showT (B.lineCount buf) <> " lines")
    labelWith (tight . fontMuted) (langName (edLang ed))
    labelWith (tight . fontMuted) (if B.usesTabs buf then "Tabs" else "Spaces")
    labelWith (tight . fontMuted) (T.pack (show (formatEol (appFormat app))))
    when (formatBom (appFormat app)) $ labelWith (tight . fontMuted) "BOM"
    when (zoom /= 100) $ labelWith (tight . fontMuted) (showT zoom <> "%")
  where
    ed = appEditor app
    buf = edBuffer ed
    name = maybe "Untitled" (T.pack . takeFileName) (appPath app)
    zoom = round (edFontSize ed / defaultFontSize * 100) :: Int
    (ln, col) = B.cursorPosition buf
    showT :: Show a => a -> Text
    showT = T.pack . show
