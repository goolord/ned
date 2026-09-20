-- | Lexical syntax highlighting, a line at a time.
--
-- This module is the door on three: "Ned.Highlight.Lang" says what a language
-- is, "Ned.Highlight.Lex" colours a line of one, and
-- "Ned.Highlight.Languages" holds the ones the editor knows. A caller that
-- only wants a file coloured imports this and needs none of that.
module Ned.Highlight
  ( -- * Lines, coloured
    TokenKind (..)
  , Span (..)
  , lexLine
  , lexState

    -- * Languages
  , Lang (..)
  , LexState (..)
  , plainText
  , languageFor
  ) where

import Ned.Highlight.Lang
import Ned.Highlight.Languages
import Ned.Highlight.Lex
