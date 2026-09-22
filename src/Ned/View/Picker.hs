-- | The fuzzy finder's panel: the prompt over the rows that answer it, with a
-- preview of the one the keyboard is on beside them, and the keys that work
-- here along the foot. It is a modal over the window, so while it is up it is
-- the only thing that reads a key.
--
-- What the finder holds, and what gathering, matching and reading a preview
-- do to it, are "Ned.Picker"'s; this is what it looks like and what the keys
-- and the pointer do. Everything is set on the editor's monospace cell grid,
-- and the preview draws its lines the way the editor does, with
-- "Ned.View.Code".
module Ned.View.Picker
  ( pickerOverlay
  ) where

import Control.Monad (unless, void, when)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Effectful (Eff, type (:>))
import NanoUI
import Ned.Editor (cellWidth, defaultFontSize)
import Ned.Highlight (TokenKind (..), langName, languageFor)
import Ned.Picker
import Ned.Text (clamp)
import Ned.Theme
import Ned.View.Code
import Ned.Widget
import System.FilePath (takeFileName)

--------------------------------------------------------------------------------
-- The panel
--------------------------------------------------------------------------------

-- | The finder over the window, as a modal: what is behind it keeps its place
-- and takes nothing, and nothing else in the frame moves when it comes and
-- goes. 'Nothing' back puts the finder away; the item is one that was picked,
-- which puts it away as well.
pickerOverlay :: Ui :> es => Maybe Picker -> Eff es (Maybe Picker, Maybe Item)
pickerOverlay mpk = do
  winW <- windowWidth
  winH <- windowHeight
  -- Whole pixels, so that the panel's edges land on the pixel grid.
  let whole v = fromIntegral (floor v :: Int)
      panelW = whole (clamp 500 1620 (winW - 40))
      panelH = whole (clamp 320 1160 (winH - 40))
  (closeResp, out) <-
    modalWith (fixedWH panelW panelH) (isJust mpk) (maybe "" (srcTitle . pkSource) mpk) $
      maybe (pure (Nothing, Nothing)) pickerBody mpk
  let (kept, chosen) = fromMaybe (Nothing, Nothing) out
      left = if respClicked closeResp then Nothing else kept
  -- Whatever put it away, the gathering thread is told to stop.
  when (isNothing left) (mapM_ (uiIO . closePicker) mpk)
  pure (left, chosen)

-- | The panel: the prompt over the rows, with the preview beside them, and
-- the keys that work here along the foot. It is set in the editor's font, at
-- the size the editor starts at.
pickerBody :: Ui :> es => Picker -> Eff es (Maybe Picker, Maybe Item)
pickerBody pk0 = do
  fm <- resolveFontUi defaultFontSize WeightNormal FontStyleNormal FontMono
  cellW <- cellWidth (lineWidthUi fm)
  columnWith (tight . gap 0 . fillW . fillH) $ do
    -- The prompt, which keeps the keyboard for as long as the finder is up.
    -- It is a search input, which says whether typing has paused -- that is
    -- when a live source is asked again -- and Enter counts as settled too:
    -- the rows from before the pause are for something else, and Enter does
    -- not open one of them.
    (typed, settled) <-
      rowWith (padXY 0 0 . tight . fillW . gap 8 . alignMid) $ do
        (resp, txt) <-
          searchInputConfigured'
            defaultSearchInputConfig {sicPlaceholder = srcPrompt (pkSource pk0), sicDebounceMs = 200}
            (pkTyped pk0)
        holdFocus (respId resp)
        labelWith (tight . fontMuted . alignMid) (counted pk0)
        pure (txt, respChanged resp || respSubmitted resp)
    pk1 <- uiIO (restock pk0 typed settled)
    -- The rule under the prompt spans the body, so where it was laid out
    -- last frame says how wide the body is, which the column of rows takes
    -- its share of.
    ruleId <- currentId
    separator
    bodyW <- maybe 900 rectW <$> lastRect ruleId
    (pk3, chosen, closed) <- rowWith (grow . gap 0 . padAll 0) $ do
      (pk2, chosen, closed) <- rowsPane fm cellW (fromIntegral (round (min (bodyW * 0.42) (60 * cellW)) :: Int)) pk1
      separator
      -- The preview: a heading that says what the file is, over the head of
      -- it in the colours the editor would open it in.
      pk3 <- uiIO (ensurePreview pk2) >>= \pkp ->
        columnWith (tight . gap 0 . grow . fillH) $ previewHeading pkp >> previewBody fm cellW pkp
      pure (pk3, chosen, closed)
    separator
    rowWith (padLRTB 0 0 8 0 . tight . fillW . gap 16 . alignMid) $
      mapM_
        ( \(k, what) -> rowWith (tight . gap 5 . alignMid) $ do
            labelWith (tight . alignMid) k
            labelWith (tight . fontMuted . alignMid) what
        )
        [ ("Enter", "open")
        , ("\x2191 \x2193", "move")
        , ("Ctrl+D  Ctrl+U", "scroll the file")
        , ("Esc", "close")
        ]
    pure (if closed || isJust chosen then Nothing else Just pk3, chosen)
  where
    -- What answered, out of what there is: @48/1203@, with the gatherer's
    -- progress while it is still running. A live source's rows all answer.
    counted pk
      | srcLive (pkSource pk) = showT (hitCount pk) <> pending
      | otherwise = showT (hitCount pk) <> "/" <> showT (pkTaken pk) <> pending
      where
        pending = if pkDone pk && pkTyped pk == pkQuery pk then "" else "\x2026"

showT :: Int -> Text
showT = T.pack . show

-- | Whether a chord of Ctrl and this letter was typed this frame.
chorded :: Input -> Char -> Bool
chorded inp c = modCtrl mods && not (modAlt mods) && T.any (== c) (inputChars inp)
  where
    mods = inputModifiers inp

--------------------------------------------------------------------------------
-- The rows
--------------------------------------------------------------------------------

-- | The rows that answer the query, in a scroller that builds only the rows
-- in its view: a list of a hundred thousand draws the twenty on screen and no
-- others, as one custom widget between two spacers that stand for the rows
-- above and below them. This is where the finder's keys are read; what comes
-- back is the finder as the frame leaves it, an item that was picked, and
-- whether the finder was put away.
rowsPane :: Ui :> es => FontMetrics -> Float -> Float -> Picker -> Eff es (Picker, Maybe Item, Bool)
rowsPane fm cellW rowsW pk0 = do
  inp <- askInput
  sid <- currentId
  -- Escape puts the finder away, unless it is the Escape that closes the
  -- prompt's own right-click menu.
  closed <- takeEscape
  metrics0 <- getScrollMetricsUi sid
  let lineH = rowHeight fm
      count = hitCount pk0
      -- Enter takes what the keyboard is on; the rest is walking the rows,
      -- with the arrows or the chords a terminal's finder walks them with.
      step = \case
        KeyUp -> -1
        KeyDown -> 1
        _ -> 0 :: Int
      moved =
        foldInputKeys (\acc k -> acc + step k) 0 (inputKeys inp)
          + fromEnum (chorded inp 'n')
          - fromEnum (chorded inp 'p')
      -- The pointer. A press takes the row under it, as a press on the file
      -- tree takes a file. The rows are where the scroller's view last put
      -- them.
      viewport = maybe (Rect 0 0 320 400) scrollViewport metrics0
      offset0 = maybe 0 (v2Y . scrollOffset) metrics0
      pointed = floor ((v2Y (inputMousePos inp) - rectY viewport + offset0) / lineH) :: Int
      onRow = rectContains viewport (inputMousePos inp) && pointed >= 0 && pointed < count
      pressed = onRow && inputMousePressed inp
      cursor = clamp 0 (max 0 (count - 1)) (if pressed then pointed else pkCursor pk0 + moved)
  -- Three rows a notch, a new query back at the top, and the row the keyboard
  -- moved to kept in view.
  setScrollStepUi sid (3 * lineH)
  when (pkToTop pk0) (scrollToUi sid (V2 0 0) ScrollInstant)
  when (moved /= 0) (scrollRectIntoViewUi sid (Rect 0 (fromIntegral cursor * lineH) 1 lineH) ScrollNearest ScrollInstant)
  offset <- maybe offset0 (v2Y . scrollOffset) <$> getScrollMetricsUi sid
  let hovered = if onRow then pointed else -1
      first = clamp 0 (max 0 (count - 1)) (floor (offset / lineH))
      last' = min (count - 1) (first + ceiling (rectH viewport / lineH))
      pk1 = pk0 {pkCursor = cursor, pkScroll = realToFrac (offset / lineH), pkHovered = hovered, pkToTop = False}
      -- Stale rows answer the last query and not the one in the prompt, so
      -- nothing is opened from them.
      chosen = if (inputKeysElem KeyEnter (inputKeys inp) || pressed) && isNothing (pkStale pk0) then currentItem pk1 else Nothing
      -- What stands in for the rows when there are none: whether nothing
      -- answered, there is nothing to answer yet, or the gatherer could not
      -- look. Text that wraps, as an error can be longer than the column.
      emptyNote
        | Just msg <- pkFailed pk1 = msg
        | srcLive (pkSource pk1) && T.null (T.strip (pkQuery pk1)) = "Type to search."
        | pkTaken pk1 == 0 && not (pkDone pk1) = "Looking\x2026"
        | T.null (pkQuery pk1) = "Nothing here."
        | otherwise = "No match for " <> pkQuery pk1
  -- A gatherer that is still running asks for frames of its own: nothing else
  -- knows that more rows have arrived.
  unless (pkDone pk1) (wakeAfter 0.03)
  -- The rows on screen, each with the characters of it the query matched.
  shown <- uiIO (smallArrayFromList <$> traverse (visibleRow pk1) [first .. last'])
  let widest = foldl' (\m (PickRow item _) -> max m (T.length (rowLead item))) 0 shown
      pk2 = pk1 {pkNameCells = max (pkNameCells pk1) (min 36 widest)}
      scene =
        RowScene
          { rsRows = shown
          , rsFirst = first
          , rsLineH = lineH
          , rsTextH = fmLineHeight fm
          , rsCellW = cellW
          , rsCursor = cursor
          , rsHovered = hovered
          , rsNameCells = pkNameCells pk2
          , -- How wide the rows may draw, which is the viewport: the scroller
            -- keeps its scrollbar's lane out of that, so a row held to it
            -- cannot run under the bar. Nothing has published a viewport
            -- before the scroller's first frame, so until then nothing is
            -- held back.
            rsMaxW = maybe (1 / 0) (rectW . scrollViewport) metrics0
          }
      key =
        contentKeyOf
          [ keyPart (pkQuery pk1), keyPart (pkTaken pk1), keyPart count, keyPart cursor, keyPart hovered
          , keyPart (pkNameCells pk2), keyPart first, keyPart last', keyPart (rsMaxW scene)
          ]
      above = fromIntegral first * lineH
      inView = fromIntegral (max 0 (last' - first + 1)) * lineH
      below = fromIntegral (max 0 (count - last' - 1)) * lineH
  _ <-
    scrollArea (tight . gap 0 . fillH . fixedW rowsW) $
      if count <= 0
        then scope $ columnWith (padXY (rowMark + rowPad) 6 . tight . fillW) $
          void (richTextWith (fillW . fontMuted) [inlineText emptyNote])
        else scope $ do
          spacer Fit (Fixed above)
          _ <-
            customWidget
              defaultCustomWidgetSpec
                { widgetLayout = (fillW . fixedH inView) defaultLayout
                , widgetDraw = \cdc r -> drawRows cdc scene r
                , widgetContent = key
                , widgetCursor = Just (const UiCursorDefault)
                , widgetDamageSlop = 0
                , -- Every row in view is this one widget, so the row under a
                  -- moving pointer keeps up only with a frame for every move
                  -- over it.
                  widgetTrackPointer = True
                }
          spacer Fit (Fixed below)
  pure (pk2, chosen, closed)
  where
    visibleRow pk i = case hitItem pk i of
      Nothing -> pure (PickRow (Item "" "" Nothing U.empty []) U.empty)
      Just item -> PickRow item <$> matchedChars pk item

-- | A row as it is drawn: what it is, and which of its characters answered
-- the query.
data PickRow = PickRow !Item !(U.Vector Int)

-- | Everything the rows' drawing reads.
data RowScene = RowScene
  { rsRows :: !(SmallArray PickRow)
  , rsFirst :: !Int
  -- ^ The hit the first of the rows is.
  , rsLineH :: !Float
  , rsTextH :: !Float
  -- ^ A line of the font the rows are set in.
  , rsCellW :: !Float
  , rsCursor :: !Int
  , rsHovered :: !Int
  , rsNameCells :: !Int
  , rsMaxW :: !Float
  -- ^ How wide the rows may draw: the scroller's viewport.
  }

-- | The draw ops of the rows in view, the first of them at the top of the
-- widget. Every character is its own op on its own cell: the toolkit puts a
-- run's first glyph on the pixel grid and the rest at whole pixels from it,
-- so a glyph whose run started elsewhere this frame would step a pixel
-- sideways. Placed on its own, a glyph is where its cell is and nowhere else.
drawRows :: CustomDrawContext -> RowScene -> Rect -> SmallArray DrawOp
drawRows cdc sc rect@(Rect x y w _) =
  smallArrayFromList (FillRect rect (tcPanel tc) : concatMap rowOps [0 .. sizeofSmallArray (rsRows sc) - 1])
  where
    theme = cdcTheme cdc
    tc = treeColors theme
    lineH = rsLineH sc
    cellW = rsCellW sc
    -- A row's band and its text answer to the viewport rather than to the
    -- widget, so neither can be wider than the column the rows sit in nor
    -- reach the scrollbar beside it. The panel behind them still fills the
    -- widget, or its edge would be a seam down the lane.
    roomW = min w (rsMaxW sc)
    rowFont = codeFont defaultFontSize TokPlain

    rowOps j =
      let PickRow item pos = indexSmallArray (rsRows sc) j
          i = rsFirst sc + j
          ry = y + fromIntegral j * lineH
          rowRect = Rect x ry roomW lineH
          picked = i == rsCursor sc
          backdrop
            | picked = [rowBand rowRect (tcPicked tc)]
            | i == rsHovered sc = [rowBand rowRect (tcHover tc)]
            | otherwise = []
          -- The mark down the left of the row the keyboard is on is the
          -- caret's colour.
          mark = [markOp rowRect (tcCurrent tc) | picked]
          ix = x + rowMark + rowPad
          tx = ix + rowIcon
          ty = ry + (lineH - rsTextH sc) / 2
          cells = max 0 (floor ((roomW - (tx - x) - rowPad) / cellW))
          -- The name first, since it is what is being looked for, and the
          -- folder beside it in a column of its own. A folder too long for
          -- what is left loses its front: the end of it is the part nearest
          -- the file. A grep hit is the other way about: where it was found
          -- is muted, the line it found loses its end.
          hitRow = isJust (itemLine item)
          name = rowLead item
          base = T.length (itemText item) - T.length name
          folder = rowTrail item
          folderCell = max (rsNameCells sc + 3) (T.length name + 3)
          (folderShown, dropped)
            | hitRow = (T.take (cells - folderCell) folder, 0)
            | otherwise = clipFront (cells - folderCell) folder
          matched k = U.elem k pos
          nameColor k
            | hitRow = tcMuted tc
            | matched (base + k) = themeYellow theme
            | otherwise = tcName tc
          folderColor k
            | matched (k + dropped) = themeYellow theme
            | hitRow = tcName tc
            | otherwise = tcMuted tc
          glyphs cell0 txt colorOf =
            [ DrawTextStyled (tx + fromIntegral (cell0 + c) * cellW) ty rowFont (T.singleton ch) (colorOf c)
            | (c, ch) <- zip [0 :: Int ..] (T.unpack txt)
            , ch /= ' '
            ]
       in backdrop
            ++ mark
            ++ fileIcon ix (ry + lineH / 2) (languageTint theme (itemPath item))
            ++ glyphs 0 (T.take cells name) nameColor
            ++ glyphs folderCell folderShown folderColor

-- | What a row shows first, a file's name or the file and line a grep hit is
-- at; and what it shows beside that, the folder a file is in or the line a
-- grep hit is on. A path the rows show is written with forward slashes.
rowLead, rowTrail :: Item -> Text
rowLead item = case itemLine item of
  Nothing -> snd (T.breakOnEnd "/" (itemText item))
  Just ln -> T.pack (takeFileName (itemPath item)) <> ":" <> showT (ln + 1)
rowTrail item = case itemLine item of
  Nothing -> T.dropEnd 1 (fst (T.breakOnEnd "/" (itemText item)))
  Just _ -> itemText item

-- | A text cut to a number of cells, losing its front rather than its end,
-- and how many characters went. What is left starts with an ellipsis, which
-- takes a cell of its own.
clipFront :: Int -> Text -> (Text, Int)
clipFront cells txt
  | cells <= 0 = ("", 0)
  | T.length txt <= cells = (txt, 0)
  | cells <= 1 = ("\x2026", T.length txt)
  | otherwise = ("\x2026" <> T.takeEnd (cells - 1) txt, T.length txt - (cells - 1) - 1)

--------------------------------------------------------------------------------
-- The preview
--------------------------------------------------------------------------------

-- | Where the file is and what it is, with its length and its language as the
-- status bar says them. The path is worked out from the file rather than
-- taken from the row, which for a grep hit is a line of code.
previewHeading :: Ui :> es => Picker -> Eff es ()
previewHeading pk =
  rowWith (padXY 10 6 . tight . fillW . gap 12 . alignMid) $ case currentItem pk of
    Nothing -> labelWith (tight . fontMuted . alignMid) " "
    Just item -> do
      let (folder, name) = T.breakOnEnd "/" (relative (pkRoot pk) (itemPath item))
          pv = pkPreview pk
          n = V.length (pvWhole pv)
          lines'
            | pvMore pv = "More than " <> showT n <> " lines"
            | n == 1 = "1 line"
            | otherwise = showT n <> " lines"
      rowWith (tight . gap 0 . alignMid) $ do
        unless (T.null folder) $ labelWith (tight . fontMuted . alignMid) folder
        labelWith (tight . fontSemiBold . alignMid) name
      flex
      when (T.null (pvNote pv) && pvOf pv == Just (itemPath item)) $ do
        labelWith (tight . fontMuted . alignMid) lines'
        labelWith (tight . fontMuted . alignMid) (langName (languageFor (itemPath item)))

-- | The lines of the file, in a scroller on both axes: the wheel, Ctrl+D and
-- Ctrl+U move it. A preview that has just been read opens on what it is
-- about: the top of a file, or the line a grep found, far enough along that
-- line to show what was found. What stands in for the lines wraps.
previewBody :: Ui :> es => FontMetrics -> Float -> Picker -> Eff es Picker
previewBody fm cellW pk0
  | not (T.null (pvNote pv)) = scope $ do
      columnWith (padXY rowPad 6 . tight . grow . fillH) $
        void (richTextWith (fillW . fontMuted) [inlineText (pvNote pv)])
      pure pk0
  | otherwise = scope $ do
      inp <- askInput
      sid <- currentId
      metrics <- getScrollMetricsUi sid
      let lineH = lineHeight fm
          lns = pvLines pv
          count = sizeofSmallArray lns
          digits = max 2 (length (show (max 1 (pvFirst pv + count))))
          gutterW = fromIntegral (digits + 2) * cellW
          Rect _ _ viewW viewH = maybe (Rect 0 0 400 400) scrollViewport metrics
          contentW = max viewW (gutterW + fromIntegral (pvWidest pv) * cellW + rowPad)
          contentH = max viewH (fromIntegral count * lineH)
          -- Where a preview that has just been read opens: the hit a third of
          -- the way down, and the first thing found on it a third of the way
          -- along, when it is further along than the view reaches.
          hitAt = case pvHit pv of
            Just ln | ln - pvFirst pv >= 0, ln - pvFirst pv < count -> Just (ln - pvFirst pv)
            _ -> Nothing
          openY = maybe 0 (\i -> max 0 ((fromIntegral i - viewH / lineH / 3) * lineH)) hitAt
          openX = case hitAt of
            Just i
              | PreviewLine _ _ ((a, b) : _) <- indexSmallArray lns i
              , gutterW + fromIntegral b * cellW > viewW ->
                  max 0 (fromIntegral a * cellW - (viewW - gutterW) / 3)
            _ -> 0
      when (pvScroll pv < 0) (setScrollOffsetUi sid (V2 openX openY))
      when (chorded inp 'd') (scrollPagesUi sid (V2 0 0.5) ScrollInstant)
      when (chorded inp 'u') (scrollPagesUi sid (V2 0 (-0.5)) ScrollInstant)
      setScrollStepUi sid (3 * lineH)
      offX <- maybe 0 (v2X . scrollOffset) <$> getScrollMetricsUi sid
      let scene =
            CodeScene
              { csPreview = pv
              , csLineH = lineH
              , csCellW = cellW
              , csGutterW = gutterW
              , csScrollX = clamp 0 (max 0 (contentW - viewW)) offX
              , csViewW = viewW
              }
      _ <-
        scrollArea2D (tight . gap 0 . grow . fillH) $
          customWidget
            defaultCustomWidgetSpec
              { widgetLayout = fixedWH contentW contentH defaultLayout
              , widgetDraw = \_ r -> drawCode scene r
              , widgetContent = contentKeyOf [keyPart (pvVersion pv), keyPart (csScrollX scene), keyPart viewW]
              , widgetCursor = Just (const UiCursorDefault)
              , widgetDamageSlop = 0
              }
      pure pk0 {pkPreview = pv {pvScroll = 0}}
  where
    pv = pkPreview pk0

-- | Everything the preview's drawing reads.
data CodeScene = CodeScene
  { csPreview :: !Preview
  , csLineH :: !Float
  , csCellW :: !Float
  , csGutterW :: !Float
  , csScrollX :: !Float
  -- ^ How far the view is scrolled along the lines. The numbers are drawn
  -- down the left of the view there, and only the cells in view are drawn.
  , csViewW :: !Float
  }

-- | The draw ops of the preview: every line of the file that was read, in the
-- colours the editor sets them in, and the numbers down the left of the view
-- over whatever is scrolled under them.
drawCode :: CodeScene -> Rect -> SmallArray DrawOp
drawCode sc rect@(Rect x y w h) =
  smallArrayFromList (FillRect rect colBackground : concatMap lineOps [0 .. count - 1] ++ gutter)
  where
    pv = csPreview sc
    count = sizeofSmallArray (pvLines pv)
    lineH = csLineH sc
    lineY i = y + fromIntegral i * lineH
    gx = x + csScrollX sc
    firstCell = max 0 (floor (csScrollX sc / csCellW sc) - 1)
    textGrid = Grid (x + csGutterW sc) (csCellW sc) defaultFontSize firstCell (firstCell + ceiling (csViewW sc / csCellW sc) + 2)
    isHit i = pvHit pv == Just (pvFirst pv + i)

    -- A line, over the band of the grep's line and what the grep found on it,
    -- marked as Ctrl+F marks a match.
    lineOps i =
      let PreviewLine txt spans found = indexSmallArray (pvLines pv) i
       in [FillRect (Rect x (lineY i) w lineH) colCurrentLine | isHit i]
            ++ concat [cellBand textGrid (lineY i) lineH colFindMatch a b | (a, b) <- found]
            ++ codeOps textGrid (lineY i) txt spans

    gutter =
      FillRect (Rect gx y (csGutterW sc) h) colGutter
        : [lineNumber textGrid (gx + csGutterW sc) (lineY i) (isHit i) (pvFirst pv + i) | i <- [0 .. count - 1]]
