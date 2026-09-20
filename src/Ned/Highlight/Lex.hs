-- | The lexer: one line at a time, given the state the line before it ended
-- in.
--
-- Nothing here knows any particular language. It is handed a 'Lang' and
-- follows it, so the view can colour the lines on screen and no others, and
-- a line is lexed the same wherever the view happens to be scrolled to.
module Ned.Highlight.Lex
  ( lexLine
  , lexState
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Ned.Highlight.Lang

-- | The spans of a line and the state the next line starts in.
lexLine :: Lang -> LexState -> Text -> ([Span], LexState)
lexLine lang st0 line
  | langHeadings lang && st0 == LexNormal && T.isPrefixOf "#" line = ([Span (T.length line) TokKeyword], LexNormal)
  | otherwise = go st0 False sig0 line []
  where
    done acc st = (reverse acc, st)

    push 0 _ acc = acc
    push n k (Span m k' : acc) | k == k' = Span (n + m) k : acc
    push n k acc = Span n k : acc

    -- Besides the state, the lexer carries what tells the head of an
    -- application from its arguments: whether the token before was an operand
    -- (@prev@), and whether this is the type of a signature (@sig@), where
    -- what is applied is a type.
    go st _ _ t acc
      | T.null t = done acc st
    go (LexBlock depth) prev sig t acc =
      case langBlockComment lang of
        Nothing -> done (push (T.length t) TokComment acc) LexNormal
        Just (open, close) ->
          let (preC, restC) = T.breakOn close t
              (preO, restO) = T.breakOn open t
           in if langNestedComments lang && not (T.null restO) && T.length preO < T.length preC
                then
                  let n = T.length preO + T.length open
                   in go (LexBlock (depth + 1)) prev sig (T.drop n t) (push n TokComment acc)
                else
                  if T.null restC
                    then done (push (T.length t) TokComment acc) (LexBlock depth)
                    else
                      let n = T.length preC + T.length close
                          st = if depth <= 1 then LexNormal else LexBlock (depth - 1)
                       in go st prev sig (T.drop n t) (push n TokComment acc)
    go (LexString delim) _ sig t acc =
      let (pre, rest) = T.breakOn delim t
       in if T.null rest
            then done (push (T.length t) TokString acc) (LexString delim)
            else
              let n = T.length pre + T.length delim
               in go LexNormal True sig (T.drop n t) (push n TokString acc)
    go (LexGap q) _ sig t acc =
      let n = T.length (T.takeWhile isSpace t)
       in case T.uncons (T.drop n t) of
            -- A gap is any white space, blank lines among it.
            Nothing -> done (push n TokPlain acc) (LexGap q)
            Just ('\\', _) -> string q sig (T.drop n t) (push n TokPlain acc)
            -- No gap after all: the line is code, and a line of its own. Only
            -- the start of a line is lexed in this state.
            Just _ -> lexLine lang LexNormal line
    go LexNormal prev sig t acc =
      case T.uncons t of
        Nothing -> done acc LexNormal
        Just (c, rest)
          | isSpace c ->
              let n = T.length (T.takeWhile isSpace t)
               in go LexNormal prev sig (T.drop n t) (push n TokPlain acc)
          -- Before the line comments: Lua's block comment opens with its
          -- line comment's marker.
          | Just (open, _) <- langBlockComment lang
          , open `T.isPrefixOf` t ->
              let n = T.length open
               in go (LexBlock 1) prev sig (T.drop n t) (push n TokComment acc)
          | any (`T.isPrefixOf` t) (langLineComments lang) ->
              done (push (T.length t) TokComment acc) LexNormal
          | Just delim <- firstPrefix (langMultiStrings lang) t ->
              let n = T.length delim
               in go (LexString delim) prev sig (T.drop n t) (push n TokString acc)
          | c `elem` langStrings lang -> string c sig t acc
          | c == '\'' && langCharLiterals lang ->
              case charLiteralLength t of
                Just n -> go LexNormal True sig (T.drop n t) (push n TokString acc)
                Nothing -> go LexNormal prev sig rest (push 1 TokPlain acc)
          | isDigit c ->
              let n = T.length (T.takeWhile (\x -> isAlphaNum x || x == '.' || x == '_') t)
               in go LexNormal True sig (T.drop n t) (push n TokNumber acc)
          | c == '#' && langDirectives lang ->
              let n = 1 + T.length (T.takeWhile isAlpha rest)
               in go LexNormal False sig (T.drop n t) (push n TokKeyword acc)
          | isAlpha c || c == '_' ->
              let word = T.takeWhile isIdent t
                  n = T.length word
                  after = T.drop n t
                  kind
                    | word `Set.member` langKeywords lang = TokKeyword
                    | word `Set.member` langTypes lang = TokType
                    | langQualifiers lang && isUpper c && (qualifies after || importHead t) = TokModule
                    | langCapitalTypes lang && isUpper c = TokType
                    | langCalls lang && T.isPrefixOf "(" (T.stripStart after) = TokFunction
                    | langApplication lang && (lineStart acc || not prev && not sig && applied after) = TokFunction
                    | otherwise = TokPlain
                  -- A qualifier is part of the name after it, which is an
                  -- operand or not as it would be alone.
                  prev' = case kind of
                    TokKeyword -> False
                    TokModule -> prev
                    _ -> True
               in go LexNormal prev' sig after (push n kind acc)
          | c == '.'
          , Span _ TokModule : _ <- acc ->
              go LexNormal prev sig rest (push 1 TokModule acc)
          | otherwise ->
              -- A closing bracket ends an operand, and what follows a lambda's
              -- backslash are its parameters.
              let prev' = c `elem` [')', ']', '}', '\\']
                  sig' = sig || langApplication lang && c == ':' && T.isPrefixOf ":" rest
               in go LexNormal prev' sig' rest (push 1 TokPunct acc)

    -- A line that carries on a signature starts inside its type.
    sig0 = langApplication lang && any (`T.isPrefixOf` T.stripStart line) ["->", "=>", "::"]

    lineStart acc = null acc && st0 == LexNormal

    importLine = langQualifiers lang && st0 == LexNormal && T.takeWhile isIdent (T.stripStart line) `elem` ["import", "module"]

    -- Whether the rest of the line starts before the import's list.
    importHead t = importLine && T.length t > T.length (T.dropWhile (/= '(') line)

    qualifies after = case T.unpack (T.take 2 after) of
      ['.', x] -> isAlpha x || x == '_'
      _ -> False

    -- Whether an argument follows: the start of an operand that is no keyword.
    applied after =
      let next = T.stripStart after
       in T.isPrefixOf " " after && case T.uncons next of
            Just (x, _) ->
              (isAlphaNum x || x `elem` ['_', '"', '(', '[', '\\'])
                && not (T.takeWhile isIdent next `Set.member` langKeywords lang)
            Nothing -> False

    isIdent x = isAlphaNum x || x == '_' || x `elem` langIdentExtra lang

    firstPrefix ds t = case filter (`T.isPrefixOf` t) ds of
      d : _ -> Just d
      [] -> Nothing

    -- A string at the start of @t@: the character that opens or resumes it,
    -- and then its body.
    string q sig t acc =
      let (m, gap) = stringLength q (T.drop 1 t)
          n = 1 + m
       in if gap && langStringGaps lang
            then done (push n TokString acc) (LexGap q)
            else go LexNormal True sig (T.drop n t) (push n TokString acc)

    -- Length of a string's body and closing quote, escapes skipped over, and
    -- whether a backslash ending the line broke it off: one with nothing
    -- after it but white space, which is the gap's. A string left open ends
    -- with the line.
    stringLength q = scan 0
      where
        scan !n t = case T.uncons t of
          Nothing -> (n, False)
          Just ('\\', r)
            | T.all isSpace r -> (n + 1 + T.length r, True)
            | otherwise -> scan (n + 2) (T.drop 1 r)
          Just (x, r)
            | x == q -> (n + 1, False)
            | otherwise -> scan (n + 1) r

    charLiteralLength t = case T.unpack (T.take 4 t) of
      ['\'', x, '\'', _] | x /= '\\' -> Just 3
      ['\'', x, '\''] | x /= '\\' -> Just 3
      '\'' : '\\' : _ ->
        let body = T.takeWhile (/= '\'') (T.drop 2 t)
            n = 2 + T.length body + 1
         in if n <= 12 && T.length t >= n then Just n else Nothing
      _ -> Nothing

-- | The state a line leaves behind, without its spans.
lexState :: Lang -> LexState -> Text -> LexState
lexState lang st line = snd (lexLine lang st line)
