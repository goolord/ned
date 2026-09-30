-- | One frame of the window, and the row its two panes share.
--
-- From the top: what the frame answers to, the title bar the menus are along,
-- the tree and the editor side by side in their pane grid, the find bar under
-- them, the status bar under that, and the overlays over the lot: the fuzzy
-- finder, the command palette, and the question about unsaved changes. The
-- window keeps no title
-- bar of the desktop's, so the one along the top is its own:
-- it carries the name and the three buttons, and what the desktop drags and
-- resizes the window by is handed over from there.
--
-- What each of those is made of is one module down -- the editor's widgets in
-- "Ned.View.Editor", the tree's in "Ned.View.Tree", the finder's in
-- "Ned.View.Picker", the bars, the menus and the window's own chrome in
-- "Ned.View.Chrome" -- so what is left here is the order they go in and the
-- room each of them gets.
--
-- What a key does to the text is in "Ned.Editor.Keys", what a frame of the
-- editor works out from its input in "Ned.Editor", the tree's in
-- "Ned.FileTree", what the application can be asked to do in
-- "Ned.App.Commands" and what it is between frames in "Ned.App.State"; every
-- button and menu row here asks for one of those, so what a thing says and
-- what it does sit on the same line.
module Ned.View
  ( appView
  , blankView
  , tracedView
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when)
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.NanoRope.Measured as Rope
import Data.Traversable (mapAccumL)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend.Sdl (openUrl)
import qualified NanoUI as G (GridNode (..))
import NanoUI.Markdown (Block (CodeBlock), MarkdownConfig (..), MarkdownDoc, appendMarkdown, defaultMarkdownConfig, markdownConfigured, parseMarkdown)
import Ned.App.Commands
import Ned.App.Frame
import Ned.App.State
import qualified Ned.App.State as S
import Ned.Complete.Tags (Tags, noTags, watchTags)
import Ned.Config (Config (..), watchConfig)
import qualified Ned.Buffer as B
import Ned.Editor (Editor (..))
import Ned.Editor.Vim (Vim (..))
import Ned.FileTree (FileTree (..), minTreeWidth, rootName)
import Ned.Highlight (Lang, LexState (LexNormal), Span (..), TokenKind (TokPlain), languageNamed, lexLine, plainText)
import Ned.Lsp (Diagnostic (..), severityName)
import Ned.Picker (Item (..))
import Ned.Theme (colDiagnostic, paneChrome, tokenColor, tokenWeight)
import Ned.View.Chrome
import Ned.View.CommandPalette (commandPaletteOverlay)
import Ned.View.Editor (editorView)
import Ned.View.Picker (pickerOverlay)
import Ned.View.Tree (fileTreePanel)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- One frame of the window
--------------------------------------------------------------------------------

-- | The whole window, in the order things happen in: what the frame answers
-- to, then the menus, then the tree and the editor side by side, then the bar
-- under them, then the status bar, and the overlays over all of it.
--
-- The state is in an 'IORef' that nano-ui knows nothing of, so the frame reads
-- it again after each part that may have changed it, and asks for another
-- frame at the end when what it drew is no longer what the state says.
appView :: IORef App -> NanoUI ()
appView ref = do
  -- What vim's keys asked for last frame is done before the frame reads
  -- the state it runs on.
  runVimRequests ref
  takeAnswers ref
  syncServer ref
  tags <- watchedTags ref
  reloadConfig ref
  trackWindow ref
  app0 <- readApp ref

  -- A dialog that is up is asked for its answer, a file dropped on the window
  -- opens, and the chords the application owns are read, all before anything
  -- is placed: what they leave behind is what the frame goes on to draw.
  pollDialogs ref app0
  appChords ref app0

  ----------------------------------------------------------------- layout ---
  -- A tab the pointer holds says nothing yet of where it would land: the
  -- panes say so again this frame, each for its own rectangle.
  modifyApp ref rewindDrag
  drawn <- windowBorder $ columnWith (grow . gap 0 . padAll 0) $ do
    titleBar ref app0
    separator

    -- The tree and the editor run on the state as the chords and menus above
    -- left it. They are the panes of a pane grid, so the bars between them
    -- are the toolkit's to draw and drag, and the shape of the row is the
    -- state's to keep.
    app1 <- readApp ref
    -- Whether the question about unsaved changes was up as the frame found
    -- things, which is what the chords above were read under too: one that
    -- puts the question up leaves this frame's keys where they were going.
    let unblocked = not (modalUp app0) && T.null (appOpenMenu app1)
        -- The tree pane: the panel, and what a frame's clicks on it left
        -- behind. The find bar's field takes the keyboard from the tree as it
        -- does from the editor, so the arrows do not walk both at once.
        treePane respRef pctx = do
          (resp, ft, opened, asked) <-
            fileTreePanel
              (paneDragHandle pctx)
              (isJust (edVim (appEditor app1)))
              (appTreeFocus app1 && not (appBarFocus app1) && unblocked)
              (appPath app1)
              (appTree app1)
          liftIO (writeIORef respRef (Just resp))
          -- Its width last frame, which is zero before it has been laid out.
          let Rect _ _ treeW _ = pgcRect pctx
          modifyApp ref $ \a ->
            a
              { appTree = ft
              , appLayout = if treeW > 0 then (appLayout a) {layoutTreeWidth = treeW} else appLayout a
              , appTreeFocus = appTreeFocus a || ftPressed ft
              , appBarFocus = appBarFocus a && not (ftPressed ft)
              }
          -- A file the tree was clicked on opens in the tab in front, in
          -- place of what it held, or with Shift in a tab of its own; the
          -- keyboard goes to it so that it can be typed into at once.
          shifted <- modShift . inputModifiers <$> askInput
          for_ opened $ \path -> do
            modifyApp ref (\a -> a {appTreeFocus = False})
            if shifted then openFile ref Nothing path else openHere ref path
          -- What the tree's vim keys asked the application goes in the
          -- editor's vim's own queue, and is done a frame later for the same
          -- reason: so that the keys that put the finder up are not typed
          -- into it as well.
          for_ asked (queueVimRequest ref)
          when (not (null asked)) requestFrame
          -- The header is the pane's drag handle, so a hold on the root's
          -- name drags the pane and nothing else in it does.
          pure (PaneView (rootName ft) False)
        -- A pane shows its strip when there is more than the one tab in it,
        -- or more than the one pane to take a tab to.
        showsStrip a pt = length (paneDocs pt) > 1 || not (null (appPanes a))
        -- An editor pane: the strip of the files it holds, and the text of
        -- the one in front of them. A press on the text makes the pane the
        -- one in front; what a release over its body would do with a tab
        -- held -- a move into this pane, when it came from another -- the
        -- pane says while the pointer is over it.
        editorPane respRef pid pctx = do
          columnWith (grow . gap 0 . padAll 0) $ do
            -- Scoped so that the editor keeps its ids whether or not the
            -- tabs are there: one file has none.
            scope $ do
              appTabs <- readApp ref
              for_ (paneOf pid appTabs) $ \pt ->
                when (showsStrip appTabs pt) (docTabs ref pid pt)
            -- The tree or a strip may have just brought another file to the
            -- front, which is this pane's buffer now if this is the pane in
            -- front. Who has the keyboard is read from before either ran,
            -- though: the keys of this frame are theirs, and an Enter that
            -- opened a file in the tree is not one to put a newline in the
            -- file it opened.
            beforePress <- readApp ref
            inp <- askInput
            for_ (paneOf pid beforePress) $ \pt ->
              when
                ( pid /= appPaneKey beforePress
                    && isJust (docMarkdownPreviewOf (paneFront pt))
                    && pressedIn MouseLeft inp
                    && rectContains (pgcRect pctx) (inputMousePos inp)
                ) (modifyApp ref (focusPane pid))
            appNow <- readApp ref
            for_ (paneOf pid appNow) $ \pt -> do
              let doc = paneFront pt
                  sourceKey = fromMaybe (docKey doc) (docMarkdownPreviewOf doc)
                  sourceDoc = find ((== sourceKey) . docKey) (appDocs appNow)
                  ed0 = maybe (docEditor doc) docEditor sourceDoc
                  focused = pid == appPaneKey appNow
                  isPreview = isJust (docMarkdownPreviewOf doc)
                  wantFocus = focused && not isPreview && not (appBarFocus app1) && not (appTreeFocus app1) && unblocked
                  marks = if focused then findMarks appNow else (B.Matching False False, "")
                  diags = if focused then diagnosticSpans appNow else []
              when
                ( not isPreview
                    && appRequestMarkdownPreview appNow == Just (docKey doc)
                    && not (any ((== Just (docKey doc)) . docMarkdownPreviewOf) (appDocs appNow))
                ) $ do
                  when (pgcMaximized pctx) (pgcRestore pctx)
                  previewPaneKey <- pgcSplit pctx AxisV
                  let previewDoc = Doc (appNextKey appNow) ed0 Nothing (docFormat doc) (Just (docKey doc))
                  modifyApp ref $ \a ->
                    a
                      { appNextKey = appNextKey a + 1
                      , appPanes = S.Pane previewPaneKey previewDoc [] [] : appPanes a
                      , appRequestMarkdownPreview = Nothing
                      , appMarkdownPreview = True
                      }
              (mResp, ed, caret) <- case docMarkdownPreviewOf doc of
                Just previewSource -> do
                  markdownPreview ref previewSource ed0
                  pure (Nothing, ed0, Rect 0 0 0 0)
                Nothing -> do
                  (resp, updated, caretRect') <-
                    editorView (docKey doc) wantFocus marks diags (otherWords tags appNow) ed0
                  pure (Just resp, updated, caretRect')
              for_ mResp $ \resp -> liftIO (modifyIORef' respRef (++ [(pid, resp)]))
              when (not isPreview && any (not . null . vimRequests) (edVim ed)) requestFrame
              when (not isPreview && edPressed ed && not focused) (modifyApp ref (focusPane pid))
              for_ mResp $ \_ ->
                modifyApp ref $ \a ->
                  let a' = onPane pid (\p -> p {paneFront = (paneFront p) {docEditor = ed}}) a
                   in a'
                        { appBarFocus = appBarFocus a' && not (edPressed ed)
                        , appTreeFocus = appTreeFocus a' && not (edPressed ed)
                        }
              -- What a release over this pane's body would do with a tab
              -- held that came from another pane.
              modifyApp ref (noteDrop pid (pgcRect pctx))
              when (focused && not isPreview) (hoverPopup ref caret)
            -- A pane the tabs have emptied closes here, its own pane's to
            -- close, which leaves the row to the panes beside it.
            appClosed <- readApp ref
            when (appClosePane appClosed == Just pid) $ do
              pgcClose pctx
              modifyApp ref (\a -> a {appClosePane = Nothing})
            pure (PaneView (maybe T.empty (docName . paneFront) (paneOf pid appNow)) False)
    -- What each pane hangs its menu on, which the grid's own response does
    -- not carry out of it.
    treeResp <- liftIO (newIORef Nothing)
    edResps <- liftIO (newIORef [])
    gridResp <- treeEditorGrid ref (treePane treeResp) (editorPane edResps)
    -- The frame the drag's release lands on: the tab goes where the strips
    -- and panes said, or into the pane the grid proposes for it, or back
    -- where it came from. Any other frame of the drag is kept as it is.
    landHeldTabUi ref (gridResp >>= pgrDropTarget)
    app2 <- readApp ref

    -- Scoped so that the editor's own menu below keeps its ids whether or not
    -- the tree hangs its own menu this frame.
    scope $ liftIO (readIORef treeResp) >>= traverse_ (`contextMenu` treeMenu ref app2)
    -- The editor's menu is the pane in front's: what it says of the text is
    -- said of the file the keyboard would type into.
    edRespList <- liftIO (readIORef edResps)
    for_ [resp | (pid, resp) <- edRespList, pid == appPaneKey app2] $
      (`contextMenu` editorMenu ref (edBuffer (appEditor app2)))

    editorBar ref app2
    separator
    statusBar =<< readApp ref
    -- A tab the pointer holds off the strips, with nowhere in them to go:
    -- the pane the grid proposes for it, lit, and the ghost of the tab by
    -- the pointer. Scoped so that the frame after it needs no ids of its
    -- own moved.
    scope $ for_ (appTabDrag app2) $ \h ->
      unless (isJust (htDrop h)) $
        for_ (gridResp >>= pgrDropTarget) (tabGhost h (appDocs app2))
    pure (editorSig app2)

  --------------------------------------------------------------- overlays ---
  -- The picker and command palette are declared whether or not they are up,
  -- above the content and under the question about unsaved changes, which an
  -- action either can put up over them.
  appP <- readApp ref
  (picker, picked) <- pickerOverlay (cfgBufferFontSize (appConfig appP)) (appPicker appP)
  modifyApp ref (\a -> a {appPicker = picker})
  -- A grep hit opens its file on the line it was found on.
  for_ picked $ \item ->
    openFile ref (itemLine item) (itemPath item)

  appPalette <- readApp ref
  (palette, chosenCommand) <- commandPaletteOverlay ref appPalette (appCommandPalette appPalette)
  modifyApp ref (\a -> a {appCommandPalette = palette})
  for_ chosenCommand paletteCommandAction

  app3 <- readApp ref
  syncTitle ref app3
  let (question, ask) = maybe ("", "") (pendingQuestion app3) (appPending app3)
  -- Its own close button is Cancel.
  (closeResp, _) <- modal (isJust (appPending app3)) "Unsaved changes" $ do
    label question
    labelWith fontMuted ask
    rowWith (fillW . gap 8) $ do
      flex
      whenM (button "Cancel") (modifyApp ref (\a -> a {appPending = Nothing}))
      whenM (button "Discard") $ do
        modifyApp ref (\a -> a {appPending = Nothing})
        traverse_ (runPending ref) (appPending app3)
  when (respClicked closeResp) (modifyApp ref (\a -> a {appPending = Nothing}))

  appEnd <- readApp ref
  when (chromeSig appEnd /= chromeSig app0 || editorSig appEnd /= drawn) requestFrame

-- | What the language server said of what is under the caret, or found
-- wrong where it is, under it, for as long as the caret and the text stay as
-- they were. Escape or a click elsewhere puts it away.
hoverPopup :: IORef App -> Rect -> NanoUI ()
hoverPopup ref caret = do
  a <- readApp ref
  let shown = [tip | Just (at, tip) <- [appHover a], at == hoverAt a]
  (resp, _) <-
    popupWith (not (null shown)) (defaultPopupConfig (AnchorRect caret)) {cfgPlacement = PlacementBelow} (fixedW 560 . maxH 420) $
      for_ shown $ \case
        TipDoc doc -> scrollWith fillW (void (markdownConfigured (codeColoured (edLang (appEditor a))) doc))
        TipDiagnostics ds -> scrollWith (tight . gap 10 . fillW) (mapM_ diagnosticTip ds)
  when (respClicked resp || (isJust (appHover a) && null shown)) (modifyApp ref (\a' -> a' {appHover = Nothing}))

-- | A diagnostic as the popup by the caret shows it: how bad it is, in the
-- colour it is underlined in, over the whole of what the server said, in
-- the code's font, which keeps the columns of a message that quotes code.
diagnosticTip :: Diagnostic -> NanoUI ()
diagnosticTip d =
  columnWith (tight . gap 2 . fillW) $ do
    labelWith (tight . fontColor (colDiagnostic (diagSeverity d))) (severityName (diagSeverity d))
    void (richTextWith (tight . fillW . fontMono) [inlineText (T.replace "\t" "    " (T.stripEnd (diagMessage d)))])

-- | Markdown with its code blocks coloured as the editor colours code: in the
-- language the fence names, or in @lang@, the file's, where it names none. A
-- fence that names a language the editor does not know is left plain.
codeColoured :: Lang -> MarkdownConfig
codeColoured lang =
  defaultMarkdownConfig
    { mdBlock = \own -> \case
        CodeBlock info code ->
          let name = T.takeWhile (not . isSpace) info
              fenced = if T.null name then lang else fromMaybe plainText (languageNamed name)
           in Nothing <$ codeBlock name fenced code
        b -> own b
    }

-- | A code block as the Markdown widget draws one, its language and a copy
-- button over the code, with the code in colour.
codeBlock :: Text -> Lang -> Text -> NanoUI ()
codeBlock name lang code = do
  theme <- uiTheme
  size <- uiFontSize
  styled (panelStyle (background (styleBg (themeInput theme)) . borderColor (themeSeparator theme) . cornerRadius 6)) $
    panelWith (padXY 10 8 . gap 4 . fillW) $ do
      scope $ rowWith (tight . fillW . alignMid) $ do
        labelWith (tight . fontMuted . fontSizeScale 0.8) name
        flex
        whenM (styled subtle (buttonWith (padXY 6 1 . fontSize (0.8 * size)) "Copy")) $
          void (setClipboard code)
      void (richTextWith (tight . fillW . fontMono) (codePieces lang code))

markdownPreview :: IORef App -> Int -> Editor -> NanoUI ()
markdownPreview ref key ed = do
  let buf = edBuffer ed
      bufferText = Rope.toText (B.bufRope buf)
  (source, doc) <- cachedMarkdown ref key (B.bufVersion buf) bufferText
  if T.null (T.strip source)
    then labelWith (grow . fillW . padAll 16 . fontMuted) "Write Markdown in the editor to see it here."
    else do
      clicked <- scrollWith (grow . fillW . padXY 16 12) (markdownConfigured (codeColoured (edLang ed)) doc)
      for_ clicked $ \url -> do
        opened <- liftIO (openUrl url)
        modifyApp ref $ \a ->
          a
            { appStatus = if opened then "Opened link" else "Could not open link"
            }

cachedMarkdown :: IORef App -> Int -> Int -> Text -> NanoUI (Text, MarkdownDoc)
cachedMarkdown ref key version source = do
  app <- readApp ref
  case Map.lookup key (appMarkdownCache app) of
    Just (cachedVersion, cachedSource, cachedDoc)
      | cachedVersion == version -> pure (cachedSource, cachedDoc)
      | otherwise -> do
          let doc
                | cachedSource == source = cachedDoc
                | cachedSource `T.isPrefixOf` source =
                    appendMarkdown (T.drop (T.length cachedSource) source) cachedDoc
                | otherwise = parseMarkdown source
          store doc
    _ -> store (parseMarkdown source)
  where
    store doc = do
      modifyApp ref (\a -> a {appMarkdownCache = Map.insert key (version, source, doc) (appMarkdownCache a)})
      pure (source, doc)

-- | Code as rich text, a piece to a span of each line. A tab is four spaces,
-- which rich text measures as it does any other run of them.
codePieces :: Lang -> Text -> [Inline]
codePieces lang = intercalate [inlineText "\n"] . snd . mapAccumL line LexNormal . T.lines . T.replace "\t" "    "
  where
    line st t = let (spans, st') = lexLine lang st t in (st', pieces t spans)
    pieces t [] = [piece TokPlain t | not (T.null t)]
    pieces t (Span n kind : rest) = let (seg, t') = T.splitAt n t in piece kind seg : pieces t' rest
    piece kind = inlineWith (fontColor (tokenColor kind) . fontWeight (tokenWeight kind))

-- | One label and nothing else, which NED_BLANK swaps the application for: it
-- tells what a frame costs nano-ui from what it costs the editor.
blankView :: NanoUI ()
blankView = label "blank"

-- | A view with what it cost logged a line a frame to the named file: the
-- time, the window's size, and the milliseconds spent building it. NED_TRACE
-- asks for this.
tracedView :: FilePath -> NanoUI () -> NanoUI ()
tracedView file body = do
  inp <- askInput
  t0 <- liftIO getMonotonicTime
  body
  t1 <- liftIO getMonotonicTime
  let Size w h = inputWindowSize inp
  liftIO (appendFile file (printf "%.4f %.0f %.0f %.3f\n" t0 w h ((t1 - t0) * 1000)))


-- | The names in the tags file for the folder the tree is on, read again as
-- it changes on a thread the frame owns. Another folder is another thread.
watchedTags :: IORef App -> NanoUI Tags
watchedTags ref = do
  root <- ftRoot . appTree <$> readApp ref
  useStream root noTags (watchTags root)

-- | Keep the window's size and whether it fills the screen, for the layout
-- saved when it closes. A window that fills the screen, or is put away,
-- keeps the size it had before. The first frame maximizes a window that was
-- closed maximized, which the desktop is only asked for once there is one.
trackWindow :: IORef App -> NanoUI ()
trackWindow ref = do
  (started, setStarted) <- useState False
  win <- askWindow
  app <- readApp ref
  let layout = appLayout app
  if not started
    then do
      setStarted True
      when (layoutMaximized layout) maximizeWindowUi
    else unless (winMinimized win) $
      modifyApp ref $ \a ->
        a
          { appLayout =
              (appLayout a)
                { layoutMaximized = winMaximized win
                , layoutSize = if winMaximized win || winFullscreen win then layoutSize (appLayout a) else winSize win
                }
          }

-- | Take up the settings each time their file changes. The file is watched
-- on a thread the frame owns, which counts its readings and wakes the window
-- with each; a frame takes up the latest reading it has not taken up yet.
reloadConfig :: IORef App -> NanoUI ()
reloadConfig ref = do
  app <- readApp ref
  for_ (appConfigPath app) $ \path -> do
    (seen, reading) <- useStream path (0 :: Int, Nothing) $ \update ->
      watchConfig path (\r -> update (\(n, _) -> (n + 1, Just r)))
    when (seen /= appConfigSeen app) $ do
      modifyApp ref (\a -> a {appConfigSeen = seen})
      for_ reading (takeReading ref)

--------------------------------------------------------------------------------
-- The row the tree and the editors share
--------------------------------------------------------------------------------

-- Two nano-ui pane grids, one inside the other, so the bars between the
-- panes are the toolkit's to draw, drag and remember. The tree is one pane
-- of the outer grid and the editors' grid is the other, so putting the tree
-- away is the outer grid's maximizing of the editors and showing it again is
-- its restore, at the width the split was left at. The editors split about
-- among themselves in the inner grid, which makes a pane of its own for a
-- tab dragged off a strip ('pgDropPane') and names it ('pgrDropPane') for
-- the tabs to fill; a pane the tabs have emptied is closed by its own pane
-- ('pgcClose'), and a pane id means the same pane to the grid and the tabs
-- for as long as both keep it.
--
-- Neither grid is a Tab stop: their own keys act on their panes, and ned's
-- widgets own the keyboard this side of them.

-- | The bar's measurements. The panes are laid out with a 'dividerW' gap
-- between them, of which the middle 'paneSpacing' is the line that is drawn;
-- the rest is grab margin, so the bar reads as the hairline the tree's old
-- splitter was without being hard to take hold of.
dividerW, paneSpacing, paneLeeway :: Float
dividerW = paneSpacing + 2 * paneLeeway
paneSpacing = 1
paneLeeway = 2

-- | The row: the tree beside the editors, split about as it was left, and
-- the editors' grid back. Each pane's content is the view given for its id,
-- and the bars between them resize them; the tree is the outer grid's pinned
-- pane, so a resized window resizes the editors, and the tree keeps the
-- width it was left at, giving way only when the window is too narrow to
-- hold it and an editor's minimum both. Putting the tree away is the outer
-- grid's maximizing of the editors, and showing it again its restore.
--
-- What comes back is the editors' grid's response, whose 'pgrDropTarget'
-- proposes a pane for a held tab.
treeEditorGrid ::
  IORef App ->
  (PaneGridCtx -> NanoUI PaneView) ->
  (Word64 -> PaneGridCtx -> NanoUI PaneView) ->
  NanoUI (Maybe PaneGridResponse)
treeEditorGrid ref treePane editorPane = do
  winW <- windowWidth
  border <- windowBorderFor <$> askWindow
  app <- readApp ref
  -- The editors' grid runs inside its pane of the outer row, and its
  -- response -- what it proposes for a held tab -- is carried out through a
  -- ref for the frame to read after the row.
  innerResp <- liftIO (newIORef Nothing)
  -- The outer grid starts from the saved arrangement, or on a first run from
  -- the window and tree widths. Its response is kept from then on, so a resize
  -- changes the editors' width and not the split, and the first-run split is
  -- not worked out again from the tree's last-drawn width each frame.
  (outer, setOuter) <- useState (layoutOuterGrid (appLayout app))
  let start = G.Split treeEditorSplit AxisV (treeShare (winW - 2 * border) (layoutTreeWidth (appLayout app))) (G.Pane treePaneId) (G.Pane editorPaneId)
      -- The pane the editors fill: the whole row while the tree is put away,
      -- beside the tree at its width while it is shown.
      editorHost pctx = do
        if appTreeShown app
          then when (pgcMaximized pctx) (pgcRestore pctx)
          else unless (pgcMaximized pctx) (pgcMaximize pctx)
        resp <- editorGrid editorPane
        liftIO (writeIORef innerResp (Just resp))
        pure (PaneView "" False)
  outerResp <-
    styled paneChrome $
      paneGrid
        defaultPaneGridConfig
          { pgSpacing = paneSpacing
          , pgMinSize = minTreeWidth
          , pgLeeway = paneLeeway
          , pgPreserveDragSize = True
          , -- The window's width is the editors' to take or give up: the
            -- tree is as wide as it was left, whatever the window does.
            pgFixedPanes = (== treePaneId)
          , pgTree = outer <|> Just start
          , pgFocusable = False
          , pgViewPane = \pid pctx -> if pid == treePaneId then treePane pctx else editorHost pctx
          }
  let outerTree = pgrTree outerResp
  setOuter outerTree
  for_ outerTree $ \gridTree ->
    modifyApp ref $ \a ->
      a {appLayout = (appLayout a) {layoutOuterGrid = Just gridTree}}
  liftIO (readIORef innerResp)

-- | The outer row's own split, which the first frame starts the row from.
treeEditorSplit :: Word64
treeEditorSplit = 3

-- | The editors' grid: the strips of tabs and the texts, split about as the
-- tabs were dragged about. It starts as the one pane, whose id is the
-- application's too; the panes it makes for dropped tabs have ids of its own
-- after that, and the tree each frame is its own, handed back for the next.
editorGrid :: (Word64 -> PaneGridCtx -> NanoUI PaneView) -> NanoUI PaneGridResponse
editorGrid editorPane = do
  (inner, setInner) <- useState Nothing
  resp <-
    styled paneChrome $
      paneGrid
        defaultPaneGridConfig
          { pgSpacing = paneSpacing
          , pgMinSize = minTreeWidth
          , pgLeeway = paneLeeway
          , pgPreserveDragSize = True
          , pgTree = inner <|> Just (G.Pane editorPaneId)
          , pgFocusable = False
          , pgViewPane = editorPane
          }
  setInner (pgrTree resp)
  pure resp

-- | A tab held off the strips, with nowhere in them to go: the pane the grid
-- proposes for it, lit, and by the pointer the ghost of the tab, as the grid
-- draws a pane it is moving itself. It passes the pointer through, so the
-- grid keeps sight of where it is. The drawing is versioned by where the
-- pointer is and the zone under it: pinned over the whole window as it is,
-- its size never changes, so without a version its ops would stay those of
-- the frame it first appeared on and the ghost would not follow the pointer.
tabGhost :: HeldTab -> [Doc] -> PaneGridDrop -> NanoUI ()
tabGhost held docs target = do
  theme <- uiTheme
  fm <- uiFontMetrics
  let V2 mx my = htPos held
      accent = themeAccent theme
      zone = pgdRect target
      ghost = Rect (mx + 12) (my + 12) 112 28
      title = maybe "Untitled" (T.take 12 . docName) (find ((== htDoc held) . docKey) docs)
      Rect zx zy zw zh = zone
      version = contentKeyOf [keyPart title, keyPart (mx, my), keyPart (zx, zy, zw, zh)]
   in void $
        drawingVersioned version (pinAt 0 0 . grow . pointer PointerPass) $ \_ ->
          smallArrayFromList
            [ FillRect zone (withAlpha accent 0.13)
            , StrokeRoundedRect zone 0 1 accent
            , FillRoundedRect ghost 2 (withAlpha accent 0.19)
            , StrokeRoundedRect ghost 2 1 (withAlpha accent 0.5)
            , DrawTextStyled
                (mx + 18)
                (my + 16 + (28 - fmLineHeight fm) / 2)
                (TextFont 0 FontRegular WeightNormal FontStyleNormal DecorationNone)
                title
                (withAlpha (styleFg (themePanel theme)) 0.6)
            ]

-- | The tree's share of the row: the width it was left at, of what the panes
-- share out. The grid has not been laid out yet, so the window's width inside
-- its border stands in for the grid's, and the gutter is left out of the
-- share, so the tree lands at its width and not a hair off.
treeShare :: Float -> Float -> Float
treeShare usable treeW
  | room <= 0 = 0.5
  | otherwise = treeW / room
  where
    room = usable - dividerW
