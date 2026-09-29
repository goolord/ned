-- | The bars around the editor: the window's own title bar along the top,
-- the tabs of the open files over the text, the find and go-to-line bar
-- under it, and the status along the bottom.
--
-- None of this edits anything itself. Every row and button asks for one of
-- "Ned.App.Commands", so what a menu says and what it does sit on the same
-- line, and the frame in "Ned.View" is left to say where the bars go.
module Ned.View.Chrome
  ( -- * The window's own chrome
    titleBar
  , windowBorder
  , windowBorderFor
  , editorMenu
  , treeMenu

    -- * The bars
  , docTabs
  , editorBar
  , statusBar
  ) where

import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.IORef (IORef)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Word (Word64)
import NanoUI
import qualified NanoUI.Adornment as A
import NanoUI.Backend.Sdl (CaptionOptions (..), defaultCaptionOptions, defaultResizeBorder, windowCaptionWith)
import NanoUI.Monad (askFrameInput)
import NanoUI.Shortcut (Shortcut)
import qualified NanoUI.Shortcut as K
import Ned.App.Commands
import Ned.App.State
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import Ned.Config (Config (..))
import Ned.Editor
import Ned.Editor.Vim (Vim (..), newVim, vimLabel)
import Ned.File (Eol (..), FileFormat (..))
import Ned.FileTree (hasParentRoot)
import qualified Ned.FileTree as FT
import Ned.Highlight (langName)
import qualified Ned.Picker as P
import Ned.Theme (closeRed, menuChrome, windowEdge)
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
  win <- askWindow
  windowFrame
    WindowFrame
      { frameWidth = windowBorderFor win
      , frameRadius = 0
      , frameColor = windowEdge theme
      }
    body

-- | How thick the line around the window is drawn, as 'windowBorder' says:
-- none for a window that fills the screen.
windowBorderFor :: WindowState -> Float
windowBorderFor win = if winMaximized win || winFullscreen win then 0 else windowBorderWidth

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
titleBar :: IORef App -> App -> NanoUI ()
titleBar ref app = do
  closing <-
    styled menuChrome $
      rowWith (titleBarPad . tight . fillW . fixedH titleBarHeight . gap 2) $ do
        taken <- menus ref app
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
  when closing (guarded ref PendingQuit)

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
menus :: IORef App -> App -> NanoUI [Rect]
menus ref app = do
  -- The pointer as it is, where a button under an open menu sees none.
  mouse <- askFrameInput
  rects <- traverse (entry mouse) (appMenus ref app)
  when (inputMouseReleased mouse && not (T.null swallow)) (setSwallow "")
  pure rects
  where
    open = appOpenMenu app
    swallow = appMenuSwallow app
    setOpen m = modifyApp ref (\a -> a {appOpenMenu = m})
    setSwallow m = modifyApp ref (\a -> a {appMenuSwallow = m})
    entry mouse (title, body) = do
      let isOpen = open == title
      btn <- menuButtonWith' fillH title isOpen
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
          onButton = rectContains (respRect btn) (inputMousePos mouse)
      when (respClicked btn && swallow /= title) (setOpen (if isOpen then "" else title))
      when (not isOpen && not (T.null open) && respHovered btn) (setOpen title)
      (popupResp, _) <- popup isOpen cfg (columnWith (tight . gap 0) body)
      when (respClicked popupResp) $ do
        setOpen ""
        when (inputMousePressed mouse && onButton) (setSwallow title)
      pure (respRect btn)

-- | A row of a menu, greyed when it does not apply; 'menuRow' always does.
-- Picking one closes the menu bar's menu, which a context menu has none of
-- and loses nothing by.
menuEntry :: IORef App -> Bool -> Text -> Maybe Shortcut -> NanoUI () -> NanoUI ()
menuEntry ref ok lbl chord action
  | not ok = menuItemDisabled lbl
  | otherwise = whenM (maybe (menuItem lbl) (menuItemShortcut lbl) chord) (modifyApp ref (\a -> a {appOpenMenu = ""}) >> action)

menuRow :: IORef App -> Text -> Maybe Shortcut -> NanoUI () -> NanoUI ()
menuRow ref = menuEntry ref True

-- | The menus along the top, and what each of their rows does. A row that
-- turns something on and off says what it would do rather than what is so:
-- the menu is read to change something, not to find out how it stands.
appMenus :: IORef App -> App -> [(Text, NanoUI ())]
appMenus ref app = [("File", fileMenu), ("Edit", editMenu), ("View", viewMenu)]
  where
    item = menuRow ref
    buf0 = edBuffer (appEditor app)
    -- A chord vim's keys have taken is not shown, nor answered to, beside
    -- its row: the row is still there to click.
    bound chord
      | isJust (edVim (appEditor app)) && chord `elem` vimChords = Nothing
      | otherwise = Just chord
    fileMenu = do
      item "New" (bound chordNew) (newFile ref)
      item "Open..." (Just chordOpen) (openDialog ref)
      item "Find File..." (bound chordFindFile) (openPicker ref P.fileSource)
      item "Save" (Just chordSave) (save ref False)
      item "Save As..." (Just chordSaveAs) (save ref True)
      menuSeparator
      item "Close Tab" (bound chordCloseTab) (closeTab ref (appDocKey app))
      item "Exit" (Just chordQuit) (guarded ref PendingQuit)
    editMenu = do
      editEntries ref buf0
      menuSeparator
      item "Find..." (Just chordFind) (openBar ref BarFind)
      item "Search in Files..." (Just chordGrep) (openPicker ref P.grepSource)
      item "Go to Line..." (Just chordGoto) (openBar ref BarGoto)
    viewMenu = do
      item
        (if appTreeShown app then "Hide File Tree" else "Show File Tree")
        (Just chordTree)
        (toggleTree ref)
      menuSeparator
      item "Zoom In" (Just chordZoomIn) (zoom ref (* 1.1))
      item "Zoom Out" (Just chordZoomOut) (zoom ref (/ 1.1))
      item "Reset Zoom" (Just chordZoomReset) (resetZoom ref)
      menuSeparator
      item
        (if edShowWhitespace (appEditor app) then "Hide Indentation Marks" else "Show Indentation Marks")
        Nothing
        (modifyApp ref (everyEditor (\e -> e {edShowWhitespace = not (edShowWhitespace (appEditor app))})))
      item
        (if isJust (edVim (appEditor app)) then "Turn Off Vim Keys" else "Turn On Vim Keys")
        Nothing
        (modifyApp ref (everyEditor (\e -> e {edVim = maybe (Just newVim) (const Nothing) (edVim (appEditor app))})))
      item
        (if B.usesTabs buf0 then "Indent with Spaces" else "Indent with Tabs")
        Nothing
        (onBuffer ref (B.setUsesTabs (not (B.usesTabs buf0))))
      item
        (if formatEol (appFormat app) == LF then "Line Endings: CRLF" else "Line Endings: LF")
        Nothing
        (modifyApp ref (\a -> a {appFormat = (appFormat a) {formatEol = if formatEol (appFormat a) == LF then CRLF else LF}}))

-- | What the Edit menu and the editor's own menu both start with.
editEntries :: IORef App -> Buffer -> NanoUI ()
editEntries ref buf = do
  menuEntry ref (B.canUndo buf) "Undo" (Just (K.ctrl <> K.key 'z')) (onBuffer ref B.undo)
  menuEntry ref (B.canRedo buf) "Redo" (Just (K.ctrl <> K.key 'y')) (onBuffer ref B.redo)
  menuSeparator
  menuRow ref "Cut" (Just (K.ctrl <> K.key 'x')) (onBufferIO ref clipboardCut)
  menuRow ref "Copy" (Just (K.ctrl <> K.key 'c')) (onBufferIO ref clipboardCopy)
  menuRow ref "Paste" (Just (K.ctrl <> K.key 'v')) (onBufferIO ref clipboardPaste)
  menuSeparator
  menuRow ref "Select All" (Just (K.ctrl <> K.key 'a')) (onBuffer ref B.selectAll)

-- | The editor's own menu, on the right button.
editorMenu :: IORef App -> Buffer -> NanoUI ()
editorMenu ref buf = do
  editEntries ref buf
  menuRow ref "Find..." (Just chordFind) (openBar ref BarFind)

-- | The file tree's own menu, on the right button.
treeMenu :: IORef App -> App -> NanoUI ()
treeMenu ref app = do
  menuEntry ref (isJust (appPath app)) "Reveal Current File" Nothing (for_ (appPath app) (onTree ref . FT.reveal))
  menuEntry ref (hasParentRoot (appTree app)) "Open Parent Folder" Nothing (onTree ref FT.parentRoot)
  menuSeparator
  menuRow ref "Collapse All" Nothing (onTree ref FT.collapseAll)
  menuRow ref "Refresh" Nothing (onTree ref FT.refresh)
  menuSeparator
  menuRow ref "Hide File Tree" Nothing (toggleTree ref)

--------------------------------------------------------------------------------
-- The tabs over the editor
--------------------------------------------------------------------------------

-- | The open files of one pane, a tab each, over the text: folder tabs, the
-- one in front opening onto the text beneath it. A click brings a file to
-- the front and gives the text the keyboard; a press carried off the tab
-- takes it up to drag, and where the drag lands -- in this strip at the
-- place it marks, in another pane, or as a pane of its own, which the pane
-- grid proposes -- is what "Ned.App.State" does when the button comes up.
-- The cross on a tab, or a middle click on it, closes it, asking first if it
-- has changes; the button after the tabs opens a new one in this pane. A
-- file with changes to save carries a dot after its name, as it does on the
-- status bar.
docTabs :: IORef App -> Word64 -> Pane -> NanoUI ()
docTabs ref pid pt = do
  let docs = map docTab (paneDocs pt)
      docTab d =
        (closableTab (docKey d) (docName d) ())
          { tabAdornments = if docDirty d then A.trailing (A.affix "\x2022") else mempty
          }
  resp <- tabBarConfigured' (TabsConfig TabContained TabTop newTab) (docKey (paneFront pt)) docs
  when (tabActive resp /= docKey (paneFront pt)) $ do
    showTab ref (tabActive resp)
    modifyApp ref (\a -> a {appTreeFocus = False, appBarFocus = False})
  for_ (tabClosed resp) (closeTab ref)
  -- A press carried off a tab takes it up past the drag threshold; the
  -- cancellation puts it back, and the release is landed once, after the
  -- panes and the grid have all said their say.
  drag <- useDrag (tabHeaders resp)
  for_ drag $ \d ->
    if dragPhase d == DragCancelled
      then modifyApp ref (\a -> a {appTabDrag = Nothing})
      else do
        let key = dragPayload d
            others = [respRect r | (k, r) <- tabHeaders resp, k /= key]
            -- Where a release would open in this strip: before the first of
            -- the other tabs the pointer is past.
            slot = insertionIndex DragAxisX (tabStripRect resp) others (dragAt d)
        modifyApp ref (holdTab key pid (dragAt d) (dragPhase d))
        for_ slot $ \i -> do
          modifyApp ref $ \a -> case appTabDrag a of
            Just h | htFrom h == pid -> a {appTabDrag = Just h {htDrop = Just (pid, i)}}
            _ -> a
          -- The mark where a release would open, over the strip: a bar in
          -- the accent, between the tabs it would come between.
          let boundary = case splitAt i others of
                (_, r : _) -> rectX r - 1
                (_, []) -> maybe 0 (\r -> rectX r + rectW r + 1) (listToMaybe (reverse others))
          when (boundary > 0) $ do
            theme <- uiTheme
            let Rect _ sy _ sh = tabStripRect resp
            scope $ void $
              drawing (pinAt 0 0 . grow . pointer PointerPass) $ \_ ->
                smallArrayFromList [FillRect (Rect (boundary - 1) sy 2 sh) (themeAccent theme)]
  where
    newTab = whenM (styled subtle (buttonWith (tight . fixedWH 28 28) "+")) (newFileIn ref pid)

--------------------------------------------------------------------------------
-- The bar under the editor
--------------------------------------------------------------------------------

-- | Find, or go to line, or nothing at all: the bar between the text and the
-- status. A press on its field takes the keyboard back from the editor or the
-- tree, and it keeps it for as long as the bar is up.
editorBar :: IORef App -> App -> NanoUI ()
editorBar ref app = case appBar app of
  BarNone -> pure ()
  BarFind ->
    barRow "Find" (appFindText app) $ \query -> do
      when (query /= appFindText app) $ do
        modifyApp ref (\a -> a {appFindText = query})
        -- Search as the query is typed, from where the selection starts.
        onBuffer ref (\b -> B.setCursor False (fst (B.selectionRange b)) b)
        findMatch ref True
      -- Centred on the bar's middle line, whose height is the taller find
      -- field's.
      let matching = appFindMatching app
      exact <- checkboxWith alignMid "Match case" (B.matchExact matching)
      word <- checkboxWith alignMid "Whole word" (B.matchWord matching)
      let matching' = B.Matching exact word
      when (matching' /= matching) (modifyApp ref (\a -> a {appFindMatching = matching'}))
      separator
      -- The buttons are subtle: a toolbar row does not want three filled
      -- grey chips, only the press and the hover to be seen.
      styled subtle $ do
        whenM (buttonWith (tight . alignMid) "Previous") (findMatch ref False)
        whenM (buttonWith (tight . alignMid) "Next") (findMatch ref True)
        whenM (buttonWith (tight . alignMid) "Close") (closeBar ref)
      enter <- pressedEnter
      shift <- heldShift
      when enter $
        if isJust (edVim (appEditor app)) then confirmFind ref shift else findMatch ref (not shift)
  BarGoto ->
    barRow "Go to line" (appGotoText app) $ \txt -> do
      when (txt /= appGotoText app) (modifyApp ref (\a -> a {appGotoText = txt}))
      separator
      go <- styled subtle (buttonWith (tight . alignMid) "Go")
      whenM (styled subtle (buttonWith (tight . alignMid) "Close")) (closeBar ref)
      enter <- pressedEnter
      when (enter || go) $
        case readMaybe (T.unpack (T.strip txt)) of
          Just n -> onBuffer ref (B.gotoLine n) >> closeBar ref
          Nothing -> setStatus ref "Not a line number"
  where
    pressedEnter = do
      inp <- askInput
      pure (inputKeysElem KeyEnter (inputKeys inp) && appBarFocus app)
    heldShift = modShift . inputModifiers <$> askInput
    -- Keep the keyboard in the bar's field while the bar has it.
    keepFocus resp = when (appBarFocus app) (holdFocus (respId resp))
    -- A bar: its name, its field, and whatever comes after the field. The
    -- name is set at full strength and semibold, the way the tree's header
    -- and the status bar's file are, so the bar reads as naming what it is
    -- for rather than as one muted place more among the grey.
    barRow name value rest = do
      separator
      rowWith (padXY 12 5 . tight . fillW . gap 8 . alignMid) $ do
        labelWith (tight . fontSemiBold . alignMid) name
        (resp, txt) <- textInput' value
        keepFocus resp
        when (respPressed resp) (modifyApp ref (\a -> a {appBarFocus = True}))
        rest txt

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
    -- Vim's mode, and the keys of a command on their way.
    for_ (edVim ed) $ \v -> do
      labelWith (tight . fontSemiBold) (vimLabel v)
      -- A space is the leader, save on the command line, where it is typed.
      let pending = vimPending v
      unless (T.null pending) $ labelWith tight (if ":" `T.isPrefixOf` pending then pending else T.replace " " "SPC " pending)
    labelWith (tight . fontMuted) (appStatus app)
    flex
    -- A file with changes to save carries a dot, the way an editor's tab
    -- does. An asterisk read as part of the name.
    labelWith (tight . fontSemiBold) (docName doc <> (if docDirty doc then " \x2022" else ""))
    labelWith tight ("Ln " <> showT (ln + 1) <> ", Col " <> showT (col + 1))
    labelWith (tight . fontMuted) (showT (B.lineCount buf) <> " lines")
    labelWith (tight . fontMuted) (langName (edLang ed))
    labelWith (tight . fontMuted) (if B.usesTabs buf then "Tabs" else "Spaces")
    labelWith (tight . fontMuted) (T.pack (show (formatEol (appFormat app))))
    when (formatBom (appFormat app)) $ labelWith (tight . fontMuted) "BOM"
    when (zoomed /= 100) $ labelWith (tight . fontMuted) (showT zoomed <> "%")
  where
    ed = appEditor app
    buf = edBuffer ed
    doc = activeDoc app
    zoomed = round (edFontSize ed / cfgBufferFontSize (appConfig app) * 100) :: Int
    (ln, col) = B.cursorPosition buf
    showT :: Show a => a -> Text
    showT = T.pack . show
