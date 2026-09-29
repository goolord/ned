-- | A searchable list of commands over the window. The command actions live
-- with the chords and menus in "Ned.App.Commands"; this is only their prompt,
-- ordering, and selection.
module Ned.View.CommandPalette
  ( commandPaletteOverlay
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Char (isAlphaNum)
import Data.IORef (IORef)
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI
import Ned.App.Commands
import Ned.App.State
import Ned.Text (clamp)

commandPaletteOverlay :: IORef App -> App -> Maybe PaletteState -> NanoUI (Maybe PaletteState, Maybe PaletteCommand)
commandPaletteOverlay ref app openState = do
  winW <- windowWidth
  winH <- windowHeight
  size <- uiFontSize
  let width = fromIntegral (floor (max 280 (min 680 (winW - 32))) :: Int)
      rowH = max 30 (size + 12)
      pageSize = 8
      commands = commandPaletteCommands ref app
      count = length commands
      panelRows = max 1 (min pageSize count)
      height = fromIntegral (floor (max 180 (min (winH - 32) (118 + rowH * fromIntegral panelRows))) :: Int)
  (closeResp, result) <-
    modalWith (fixedWH width height . gap 0 . padLRTB 14 14 12 14) (isJust openState) "Command Palette" $
      case openState of
        Nothing -> pure (Nothing, Nothing)
        Just state -> paletteBody size rowH pageSize commands state
  let (kept, chosen) = fromMaybe (Nothing, Nothing) result
      next = if respClicked closeResp || isNothing kept || isJust chosen then Nothing else kept
  pure (next, chosen)

paletteBody :: Float -> Float -> Int -> [PaletteCommand] -> PaletteState -> NanoUI (Maybe PaletteState, Maybe PaletteCommand)
paletteBody size rowH pageSize commands state = do
  let searchStyle theme = inputStyle (borderWidth 0 . cornerRadius 4 . background (themeWindow theme)) theme
      inputConfig =
        defaultTextInputConfig
          { ticPlaceholder = "Type to search commands"
          , ticLayout = (fillW . fixedH (size + 16) . fontSize size) defaultLayout
          }
  (inputResp, query) <- styled searchStyle (textInputConfigured' inputConfig (paletteQuery state))
  holdFocus (respId inputResp)
  inp <- askInput
  let moved =
        foldInputKeys
          (\n key -> n + case key of KeyUp -> -1; KeyDown -> 1; _ -> 0)
          0
          (inputKeys inp)
      ranked = rankCommands query commands
      count = length ranked
      cursor0 = if query /= paletteQuery state then 0 else paletteCursor state
      cursor = clamp 0 (max 0 (count - 1)) (cursor0 + moved)
      pageStart = clamp 0 (max 0 (count - pageSize)) (cursor - pageSize `div` 2)
      visible = zip [pageStart ..] (take pageSize (drop pageStart ranked))
      nextState = state {paletteQuery = query, paletteCursor = cursor}
  closed <- takeEscape
  clickResults <-
    if null ranked
      then do
        labelWith (tight . fillW . padXY 8 10 . fontMuted) (if T.null query then "No commands available." else "No matching commands.")
        pure []
      else
        forM visible $ \(index, cmd) -> do
          let selected = index == cursor
              rowStyle theme =
                buttonStyle
                  (if selected then background (lerpColor (themeWindow theme) (themeAccent theme) 0.14) else id)
                  theme
              rowLabel =
                (if selected then "›  " else "   ")
                  <> paletteCommandName cmd
                  <> if T.null (paletteCommandShortcut cmd) then "" else "    " <> paletteCommandShortcut cmd
          response <- styled rowStyle $ buttonWith' (fillW . fixedH rowH . padXY 8 3 . alignStart) rowLabel
          pure (if respClicked response then Just cmd else Nothing)
  let clicked = listToMaybe (catMaybes clickResults)
      submitted = if respSubmitted inputResp then atMay ranked cursor else Nothing
  rowWith (tight . fillW . gap 8 . padXY 2 5) $ do
    labelWith (tight . fontMuted . fontSizeScale 0.82) $
      if count == 0
        then "Esc to close"
        else T.pack (show (cursor + 1)) <> " of " <> T.pack (show count) <> "   ·   ↑/↓ select   Enter run   Esc close"
  pure
    ( if closed || isJust clicked || isJust submitted
        then (Nothing, submitted <|> clicked)
        else (Just nextState, Nothing)
    )

atMay :: [a] -> Int -> Maybe a
atMay xs index
  | index < 0 = Nothing
  | otherwise = case drop index xs of
      x : _ -> Just x
      [] -> Nothing

rankCommands :: Text -> [PaletteCommand] -> [PaletteCommand]
rankCommands query commands
  | T.null (T.strip query) = commands
  | otherwise =
      map third . sortOn (\(score, order, _) -> (negate score, order)) $
        [ (score, order, command)
        | (order, command) <- zip [0 :: Int ..] commands
        , Just score <- [fuzzyScore query (paletteCommandName command <> " " <> paletteCommandShortcut command)]
        ]
  where
    third (_, _, value) = value

fuzzyScore :: Text -> Text -> Maybe Int
fuzzyScore query candidate
  | null needle = Just 0
  | otherwise = walk needle hay 0 Nothing 0
  where
    needle = T.unpack (T.toCaseFold (T.strip query))
    hay = T.unpack (T.toCaseFold candidate)
    walk [] _ _ _ score = Just score
    walk _ [] _ _ _ = Nothing
    walk (wanted : rest) text position previous score = seek wanted text position previous score
      where
        seek _ [] _ _ _ = Nothing
        seek wantedChar (h : hs) at lastMatch total
          | wantedChar == h =
              let boundary = at == 0 || maybe False (not . isAlphaNum) (charBefore at hay)
                  consecutive = lastMatch == Just (at - 1)
                  points = (if boundary then 12 else 0) + (if consecutive then 9 else 0) - maybe 0 (at -) lastMatch
                  startBonus = if lastMatch == Nothing then max 0 (12 - at) else 0
               in walk rest hs (at + 1) (Just at) (total + points + startBonus)
          | otherwise = seek wantedChar hs (at + 1) lastMatch total

charBefore :: Int -> [Char] -> Maybe Char
charBefore index chars
  | index <= 0 = Nothing
  | otherwise = atMay chars (index - 1)
