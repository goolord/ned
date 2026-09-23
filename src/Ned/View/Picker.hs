-- | The fuzzy finder's panel: the prompt over the rows that answer it, with a
-- preview of the one the keyboard is on beside them, and nothing else. It is
-- a modal over the window, so while it is up it is the only thing that reads
-- a key.
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
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Effectful (Eff, type (:>))
import NanoUI
import qualified NanoUI.Adornment as A
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
  -- As much of the window as a modal is given, in whole pixels so that the
  -- panel's edges land on the pixel grid. The body starts at the rule under
  -- the title and ends at the panel's foot, with nothing between.
  let whole v = fromIntegral (floor v :: Int)
      panelW = whole (max 500 (winW - 28))
      panelH = whole (max 320 (winH - 28))
  (closeResp, out) <-
    modalWith (fixedWH panelW panelH . gap 0 . padLRTB 10 10 0 10) (isJust mpk) (maybe "" (srcTitle . pkSource) mpk) $
      maybe (pure (Nothing, Nothing)) (pickerBody (panelW - 20)) mpk
  let (kept, chosen) = fromMaybe (Nothing, Nothing) out
      left = if respClicked closeResp then Nothing else kept
  -- Whatever put it away, the gathering thread is told to stop.
  when (isNothing left) (mapM_ (uiIO . closePicker) mpk)
  pure (left, chosen)

-- | The panel, @bodyW@ wide: the prompt over the rows down the left, and the
-- heading of the file the keyboard is on over its preview down the right,
-- the two halves of it the same height so one rule runs under both. It is
-- set in the editor's font, at the size the editor starts at.
pickerBody :: Ui :> es => Float -> Picker -> Eff es (Maybe Picker, Maybe Item)
pickerBody bodyW pk0 = do
  fm <- resolveFontUi defaultFontSize WeightNormal FontStyleNormal FontMono
  cellW <- cellWidth (lineWidthUi fm)
  let rowsW = fromIntegral (round (min (bodyW * 0.42) (60 * cellW)) :: Int)
      headH = fromIntegral (round (lineHeight fm + 10) :: Int)
  rowWith (tight . gap 0 . fillW . fillH) $ do
    (pk2, chosen, closed) <- columnWith (tight . gap 0 . fixedW rowsW . fillH) $ do
      -- The prompt, which keeps the keyboard for as long as the finder is
      -- up. A live source is asked again once typing into it has paused, and
      -- Enter asks at once: the rows from before the pause are for something
      -- else, and Enter does not open one of them.
      (resp, typed) <- prompt headH pk0
      holdFocus (respId resp)
      pk1 <- uiIO (restock pk0 typed (respSubmitted resp))
      uiIO (untilSettled pk1) >>= mapM_ wakeAfter
      separator
      rowsPane fm cellW rowsW pk1
    separator
    -- The preview: a heading that says what the file is, over the head of it
    -- in the colours the editor would open it in.
    pk3 <- uiIO (ensurePreview pk2) >>= \pkp ->
      columnWith (tight . gap 0 . grow . fillH) $ do
        previewHeading headH pkp
        separator
        previewBody fm cellW pkp
    pure (if closed || isJust chosen then Nothing else Just pk3, chosen)

-- | The prompt, @h@ tall: a bare field on the panel's own colour, with a
-- magnifier before what is typed, and after it the count of what answered
-- and, while there is something to clear, a button that clears it. A press
-- on that button is its own: the field keeps the keyboard and its caret.
prompt :: Ui :> es => Float -> Picker -> Eff es (Response, Text)
prompt h pk = do
  cleared <- uiIO (newIORef False)
  -- A small button with no fill of its own, muted like the rest of what the
  -- field draws beside its text.
  let quiet t = subtle (buttonStyle (foreground (themeMuted t)) t)
      clearButton = buttonConfigured defaultButtonConfig {bcLayout = (tight . fixedWH 20 20) defaultLayout, bcAdornments = A.leading (A.iconSized 12 clearIcon)} ""
      clear = whenM (styled quiet clearButton) (uiIO (writeIORef cleared True))
  (resp, typed) <-
    styled bare $
      textInputConfigured'
        defaultTextInputConfig
          { ticPlaceholder = srcPrompt (pkSource pk)
          , ticLayout = (fillW . fixedH h . fontSize defaultFontSize) defaultLayout
          , ticAdornments =
              A.leading (A.iconSized 14 searchIcon)
                <> A.trailing (A.affix counted)
                <> (if T.null (pkTyped pk) then mempty else A.trailing (A.control clear))
          }
        (pkTyped pk)
  -- The field takes the empty prompt up as the caller's next frame.
  wasCleared <- uiIO (readIORef cleared)
  pure (resp, if wasCleared then "" else typed)
  where
    bare t = inputStyle (borderWidth 0 . cornerRadius 0 . background (themeWindow t)) t
    -- What answered, out of what there is: @48/1203@, with the gatherer's
    -- progress while it is still running. A live source's rows all answer.
    counted
      | srcLive (pkSource pk) = showT (hitCount pk) <> pending
      | otherwise = showT (hitCount pk) <> "/" <> showT (pkTaken pk) <> pending
    pending = if pkDone pk && pkTyped pk == pkQuery pk then "" else "\x2026"

showT :: Int -> Text
showT = T.pack . show

-- | The prompt's icons, drawn in whatever colour the field gives them.
searchIcon, clearIcon :: Svg
searchIcon =
  icon
    "<svg viewBox='0 0 16 16' fill='none' stroke='currentColor' stroke-width='1.6' stroke-linecap='round'>\
    \<circle cx='6.5' cy='6.5' r='4.5'/><line x1='10' y1='10' x2='14' y2='14'/></svg>"
clearIcon =
  icon
    "<svg viewBox='0 0 16 16' fill='none' stroke='currentColor' stroke-width='1.6' stroke-linecap='round'>\
    \<line x1='4' y1='4' x2='12' y2='12'/><line x1='12' y1='4' x2='4' y2='12'/></svg>"

icon :: ByteString -> Svg
icon = either (error . ("Ned.View.Picker: an icon did not parse: " <>)) id . parseSvg

-- | A scroller in a floating panel is framed in the panel's border, which
-- the rows and the preview, drawn to the scroller's edges, would paint over.
-- The rules between the parts of the panel already say where each ends.
borderless :: Ui :> es => Eff es a -> Eff es a
borderless = styled (windowStyle (borderWidth 0))

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
    borderless . scrollArea (tight . gap 0 . fillH . fixedW rowsW) $
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
-- status bar says them, @h@ tall so that it lines up with the prompt. The path is worked out from the file rather than
-- taken from the row, which for a grep hit is a line of code.
previewHeading :: Ui :> es => Float -> Picker -> Eff es ()
previewHeading h pk =
  rowWith (padXY rowPad 0 . tight . fillW . fixedH h . gap 12 . alignMid) $ case currentItem pk of
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
        borderless . scrollArea2D (tight . gap 0 . grow . fillH) $
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
