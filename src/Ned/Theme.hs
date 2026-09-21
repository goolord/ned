-- | What colour everything in the window is.
--
-- The window has two kinds of surface, and this module holds both. The text
-- is painted in fixed colours, because a file of code should look the same
-- here as it does in the editor the theme came from. The chrome around it --
-- the menus, the file tree, the bar that resizes it -- is painted in colours
-- worked out from whatever nano-ui theme is running, so it follows the
-- toolkit.
--
-- One rule crosses both and is the reason they are written down together:
-- hue in this window always stands for something. Aqua is the caret, which
-- is where you are; the token colours are what a word is. A menu that is
-- merely open, a row the pointer is over, a bar being dragged -- none of
-- those mean anything, so they are told apart by brightness and get no hue
-- at all.
module Ned.Theme
  ( -- * The editor's surface
    colBackground
  , colGutter
  , colGutterText
  , colGutterActive
  , colCurrentLine
  , colSelection
  , colFindMatch
  , colCaret
  , colTrack
  , colThumb
  , colThumbHot
  , colWhitespace
  , caretColor

    -- * Code
  , tokenColor
  , tokenWeight

    -- * The chrome
  , menuChrome
  , paneChrome
  , windowEdge
  , closeRed
  , TreeColors (..)
  , treeColors
  , languageTint
  ) where

import Data.Bits (shiftR)
import NanoUI
import Ned.Highlight (TokenKind (..), langName, languageFor)

--------------------------------------------------------------------------------
-- The editor's surface
--------------------------------------------------------------------------------

-- The colours are those of "Tomorrow Night Min", the theme the chrome has
-- ('tomorrowNightMinDarkTheme'), from
-- https://github.com/biaqat/tomorrow-min-theme-zed. The name after each is the
-- key it has there; a key the theme leaves unset takes a neighbour's colour.

rgb :: Int -> Color
rgb v = rgba v 255

rgba :: Int -> Int -> Color
rgba v a = colorRGBA (fromIntegral (v `shiftR` 16)) (fromIntegral (v `shiftR` 8)) (fromIntegral v) (fromIntegral a)

colBackground, colGutter, colGutterText, colGutterActive, colCurrentLine, colSelection, colFindMatch, colCaret, colTrack, colThumb, colThumbHot, colWhitespace :: Color
colBackground = rgb 0x1E1F21 -- editor.background
colGutter = rgb 0x1E1F21 -- editor.gutter.background
-- editor.line_number is unset, and 'hidden' (0x63666E), the key this took
-- before, is the colour for a file nobody is meant to read: 2.9 to 1 on the
-- page. A number you count down is not that. This is the same grey a step
-- brighter, 4.0 to 1, and still far under the 9.8 to 1 of the code beside it.
colGutterText = rgb 0x7A7D85
colGutterActive = rgb 0xFFFFFF -- editor.active_line_number
colCurrentLine = rgba 0x373B41 0x80 -- editor.active_line.background
colSelection = rgba 0x373B41 0xC0 -- players[0].selection
colFindMatch = rgba 0xF0C674 0x3E -- search.match_background
colCaret = rgb 0x8ABEB7 -- players[0].cursor
colTrack = rgba 0x1D1F21 0xC0 -- scrollbar.track.background
-- scrollbar.thumb.background is 0x27292C, which over the track is 1.1 to 1:
-- a thumb nobody can find is a thumb that does not work, whatever the theme
-- says. These are the two greys this module already keeps, at 2.9 and 4.0 to
-- one, so the lane reads without the bar becoming the loudest thing on the
-- page. (scrollbar.thumb.hover_background is unset.)
colThumb = rgb 0x63666E
colThumbHot = rgb 0x7A7D85
colWhitespace = rgb 0x4D5057 -- ignored (editor.invisible is unset)

-- | The caret's colour, which is what the editor says "you are here" in. The
-- file tree marks the file the editor has open in the same colour, so that
-- one hue carries one meaning across the window.
caretColor :: Color
caretColor = colCaret

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

tokenColor :: TokenKind -> Color
tokenColor = \case
  TokPlain -> rgb 0xC5C8C6 -- editor.foreground
  TokKeyword -> rgb 0xB294BB -- keyword
  TokType -> rgb 0x81A2BE -- type: unset in the theme, and this in Zed
  TokFunction -> rgb 0xDE935F -- function
  TokModule -> rgb 0xB294BB -- title, which is what Zed gives a module
  TokString -> rgb 0xB5BD68 -- string
  TokNumber -> rgb 0xDE935F -- number
  TokComment -> rgb 0x969896 -- comment
  TokPunct -> rgb 0xC5C8C6 -- punctuation, operator

tokenWeight :: TokenKind -> FontWeight
tokenWeight = \case
  TokType -> WeightSemiBold
  TokFunction -> WeightSemiBold
  _ -> WeightNormal

--------------------------------------------------------------------------------
-- The chrome
--------------------------------------------------------------------------------

-- | The menus, in grey. A menu bar button and a menu row are not drawn from
-- 'themeButton' at all: nano-ui mixes their lit background out of
-- 'themeAccent', so the accent is the only way in. Scoping it to a grey turns
-- the open menu and the row under the pointer into steps up the surface they
-- are on, and leaves the accent everywhere else alone.
--
-- A menu that is merely open stands for nothing, so it does not get any hue.
menuChrome :: Theme -> Theme
menuChrome theme = accentColor (lerpColor (themeWindow theme) (styleFg (themePanel theme)) 0.7) theme

-- | The bar between the tree and the editor, in grey. The pane grid draws the
-- bar in the accent colour, and lights it by mixing the accent in; a bar being
-- dragged means nothing, so it is given the muted grey the tree's own seam lit
-- up to and no hue at all. The pane grid's focus ring takes the same grey,
-- since it is reached with the same keyboard.
paneChrome :: Theme -> Theme
paneChrome theme = accentColor (themeMuted theme) theme

-- | The line around the whole window, which is the only edge a window with
-- no frame of its own has to show for itself. It is the seam the bars inside
-- the window are drawn in, a step brighter: the seams inside separate one
-- grey from another, and this one has whatever is on the desktop behind it
-- to hold its own against.
windowEdge :: Theme -> Color
windowEdge theme = lerpColor (themeSeparator theme) (styleFg (themePanel theme)) 0.25

-- | What the close button lights up in under the pointer. Closing the window
-- is the one thing in its chrome that cannot be taken back, so it is the one
-- piece of chrome allowed a hue, and it is a red darker and warmer than the
-- theme's own, which is a colour for code rather than for a warning.
closeRed :: Color
closeRed = rgb 0x9C2C21

-- | The file tree's colours, worked out from the theme once a frame.
--
-- The tree is chrome, so it takes the window's own colour, which is a step
-- darker than the editor's background, and the panel's text on it. It spends
-- one colour of its own, on the row whose file the editor has open, and that
-- colour is the caret's.
--
-- What tells the rows apart is weight, not colour. A folder is set semibold,
-- and takes 'tcName' while it is open and 'tcMuted' while it is shut, so that
-- brightness says which folder's contents you are looking at. A file is set
-- normal and always at full strength: files are what a pointer is aimed at,
-- so they are the brightest thing in the panel, and the folders around them
-- are scaffolding to scan past.
data TreeColors = TreeColors
  { tcPanel :: !Color
  -- ^ Behind the rows.
  , tcName :: !Color
  -- ^ A file's name, and an open folder's.
  , tcMuted :: !Color
  -- ^ A shut folder's name, and the chevrons.
  , tcCurrent :: !Color
  -- ^ The row whose file the editor has open, and the mark down its left.
  , tcSpine :: !Color
  -- ^ The rule down each level a row is deep.
  , tcHover :: !Color
  , tcPicked :: !Color
  -- ^ The row the keyboard is on, while the tree has the keyboard.
  , tcPickedAway :: !Color
  -- ^ That row once the keyboard has gone elsewhere.
  , tcThumb :: !Color
  , tcThumbHot :: !Color
  }

treeColors :: Theme -> TreeColors
treeColors theme =
  TreeColors
    { tcPanel = themeWindow theme
    , tcName = styleFg surface
    , tcMuted = themeMuted theme
    , tcCurrent = caretColor
    , -- Most of the way back toward the window behind it. A rule this quiet
      -- is enough to follow down a column, and a deep tree draws one of them
      -- for every level, so it has to stay under the names.
      tcSpine = lerpColor (themeWindow theme) (themeSeparator theme) 0.8
    , -- Three rungs of the one grey ladder. The row the keyboard is on has to
      -- stay apart from the row the pointer is merely over, and it dims rather
      -- than disappears when the keyboard goes elsewhere, so that arrowing back
      -- lands where you left off. None of them is tinted: 'themeSelection' is
      -- built from the theme's accent, and a blue slab under the aqua name of
      -- the open file put two hues in a panel that is allowed one.
      tcHover = lerpColor (themeWindow theme) (styleHoverBg surface) 0.45
    , tcPicked = lerpColor (themeSeparator theme) (styleFg surface) 0.1
    , tcPickedAway = styleHoverBg surface
    , -- 'scrollBarThumbColor' comes out at 1.5 to 1 on this panel, which is
      -- not a thumb you can find. These are 2.5 and 3.6 to one, to sit beside
      -- the editor's own bar rather than disappear next to it.
      tcThumb = lerpColor (themeWindow theme) (styleFg surface) 0.3
    , tcThumbHot = lerpColor (themeWindow theme) (styleFg surface) 0.42
    }
  where
    surface = themePanel theme

-- | The tint on a file's page in the tree: the family of language the editor
-- would open it as, which is the one thing about a file the tree already
-- knows and the name does not always say.
--
-- There are far more languages than there are colours in the theme, so they
-- share by family rather than each having one of their own; prose and
-- anything unrecognised stay grey. The tint is on the icon alone -- the names
-- stay in the grey ladder -- so the panel reads as a column of chips beside a
-- list, and not as a list in a dozen colours.
languageTint :: Theme -> FilePath -> Color
languageTint theme path = case langName (languageFor path) of
  l | l `elem` ["Haskell", "Cabal", "Nix"] -> themePurple theme
  l | l `elem` ["C", "C++", "C#", "Rust", "Go", "Zig", "Java"] -> themeOrange theme
  l | l `elem` ["JavaScript", "TypeScript", "Python", "Lua", "Shell", "PowerShell", "SQL"] -> themeYellow theme
  l | l `elem` ["JSON", "TOML", "YAML", "XML", "CSS", "HTML", "Dockerfile", "Makefile"] -> themeGreen theme
  "Markdown" -> themeRed theme
  _ -> themeMuted theme
