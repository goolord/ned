-- | What the editor keeps between frames.
--
-- An 'Editor' is the buffer, where the view is scrolled to, and a handful of
-- answers a frame works out that the next one needs: what the pointer is in
-- the middle of dragging, when the caret last moved, where the lexer had got
-- to. A frame takes one of these and gives the next one back; nothing here
-- draws or reads input.
module Ned.Editor.Types
  ( Editor (..)
  , Drag (..)
  , newEditor
  , revealCaret
  , defaultFontSize
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Ned.Buffer (Buffer)
import Ned.Highlight (Lang, LexState (..))

data Drag
  = DragNone
  | -- | Selecting text.
    DragSelect
  | -- | Selecting by words, on from the word a double click took.
    DragWords !Int !Int
  | -- | Selecting whole lines from the gutter, starting at this line.
    DragLines !Int
  | -- | Holding the scrollbar's thumb, this far below its top.
    DragThumb !Float
  | -- | Holding the sideways bar's thumb, this far right of its left.
    DragThumbX !Float
  deriving (Eq)

data Editor = Editor
  { edBuffer :: !Buffer
  , edLang :: !Lang
  , edScrollY :: !Double
  -- ^ The line at the top of the view; its fraction is how far that line is
  -- scrolled out.
  , edScrollX :: !Float
  -- ^ Pixels.
  , edFontSize :: !Float
  , edDrag :: !Drag
  , edBlinkEpoch :: !Double
  -- ^ When the caret last moved: it shows steadily from then, and blinks after.
  , edLexCache :: !(Int, Int, LexState)
  -- ^ A version of the text, a line, and the lexer state that line starts in.
  , edFind :: !Text
  -- ^ What the view marks the matches of.
  , edFindExact :: !Bool
  , edReveal :: !Bool
  -- ^ Asks the next frame to scroll the caret into view.
  , edViewLines :: !Int
  -- ^ Whole lines that fit the view, as of the last frame.
  , edWidestVer :: !Int
  -- ^ The version of the text the width scan belongs to.
  , edWidest :: !Int
  -- ^ The widest line the width scan has measured, in cells: the whole of
  -- the buffer once it has been round, and an edit starts it over.
  , edWidestScan :: !Int
  -- ^ The line the width scan is to walk from; past the last line when it
  -- has the whole of the buffer.
  , edPressed :: !Bool
  -- ^ Whether the pointer went down on the editor this frame.
  , edShowWhitespace :: !Bool
  -- ^ Whether the indentation of a line is drawn: a dot a space, a rule a tab.
  }

defaultFontSize :: Float
defaultFontSize = 15

newEditor :: Lang -> Buffer -> Editor
newEditor lang buf =
  Editor
    { edBuffer = buf
    , edLang = lang
    , edScrollY = 0
    , edScrollX = 0
    , edFontSize = defaultFontSize
    , edDrag = DragNone
    , edBlinkEpoch = 0
    , edLexCache = (-1, 0, LexNormal)
    , edFind = T.empty
    , edFindExact = False
    , edReveal = True
    , edViewLines = 1
    , edWidestVer = -1
    , edWidest = 0
    , edWidestScan = 0
    , edPressed = False
    , edShowWhitespace = True
    }

-- | Have the next frame scroll the caret into view.
revealCaret :: Editor -> Editor
revealCaret ed = ed {edReveal = True}
