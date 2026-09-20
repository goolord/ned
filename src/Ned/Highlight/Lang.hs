-- | What a language is, and what lexing a line of one comes back with.
--
-- A 'Lang' is a description and not a program: a table of what the language
-- comments, quotes and reserves, which "Ned.Highlight.Lex" reads to colour a
-- line. Adding a language is filling this record in, in
-- "Ned.Highlight.Languages", and touches no code that lexes.
module Ned.Highlight.Lang
  ( TokenKind (..)
  , Span (..)
  , LexState (..)
  , Lang (..)
  , MultiString (..)
  , quoted
  ) where

import Data.Set (Set)
import Data.Text (Text)

data TokenKind
  = TokPlain
  | TokKeyword
  | TokType
  | TokFunction
  | TokModule
  | TokString
  | TokNumber
  | TokComment
  | TokPunct
  deriving (Eq, Show, Enum, Bounded)

-- | A run of characters of one kind; the spans of a line cover it in order.
data Span = Span {spanLength :: !Int, spanKind :: !TokenKind}
  deriving (Eq, Show)

-- | What a line starts inside of.
data LexState
  = LexNormal
  | -- | A block comment, and how deeply nested.
    LexBlock !Int
  | -- | A string that runs over lines, and the delimiter that ends it.
    LexString !Text
  | -- | A string gap, as Haskell has them: a string of this quote that a
    -- backslash ending the line before broke off, and the next one resumes.
    LexGap !Char
  deriving (Eq, Show)

-- | A string that runs over lines: what opens it, and what closes it. Most
-- languages end one the way they began it; Lua opens with @[[@ and closes
-- with @]]@.
data MultiString = MultiString
  { multiOpen :: !Text
  , multiClose :: !Text
  }
  deriving (Eq, Show)

-- | A multi-line string that ends with the delimiter it starts with.
quoted :: Text -> MultiString
quoted d = MultiString d d

data Lang = Lang
  { langName :: !Text
  , langLineComments :: ![Text]
  , langBlockComment :: !(Maybe (Text, Text))
  , langNestedComments :: !Bool
  , langStrings :: ![Char]
  -- ^ Delimiters of strings that end with their line.
  , langMultiStrings :: ![MultiString]
  -- ^ Strings that run over lines.
  , langStringGaps :: !Bool
  -- ^ Whether a backslash ending a line breaks a string off until the next.
  , langCharLiterals :: !Bool
  -- ^ Whether @'x'@ is a character, where a lone @'@ may be something else.
  , langKeywords :: !(Set Text)
  , langTypes :: !(Set Text)
  , langCapitalTypes :: !Bool
  -- ^ Whether a capitalised identifier names a type.
  , langCalls :: !Bool
  -- ^ Whether an identifier before @(@ is a function.
  , langApplication :: !Bool
  -- ^ Whether a function is applied by writing its arguments after it, as in
  -- Haskell: the head of an application is a function, and so is a name a
  -- line starts with.
  , langQualifiers :: !Bool
  -- ^ Whether a capitalised identifier before a @.@ names a module, as do
  -- those of an @import@ or a @module@ line up to its list.
  , langIdentExtra :: ![Char]
  -- ^ Characters of identifiers besides letters, digits and @_@.
  , langDirectives :: !Bool
  -- ^ Whether @#word@ is a keyword, as in C.
  , langHeadings :: !Bool
  -- ^ Whether a line starting with @#@ is a heading, as in Markdown.
  }
