-- | The lexer: one line at a time, given the state the line before it ended
-- in.
--
-- Nothing here knows any particular language. It is handed a 'Lang' and
-- follows it, so the view can colour the lines on screen and no others, and
-- a line is lexed the same wherever the view happens to be scrolled to.
--
-- A line is walked by 'Lex', which carries the scan over it: what is left,
-- the spans so far, and the two things the tokens before settle. So a branch
-- says what it takes and not where that leaves everything, and what it leaves
-- alone it does not mention.
module Ned.Highlight.Lex
  ( lexLine
  , lexState
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Ned.Highlight.Lang

-- | Where the lexer has got to in a line.
data Scan = Scan
  { scanRest :: !Text
  -- ^ What is left of the line.
  , scanSpans :: ![Span]
  -- ^ The spans taken so far, backwards.
  , scanPrev :: !Bool
  -- ^ Whether the token before was an operand, which is what tells the head
  -- of an application from its arguments.
  , scanSig :: !Bool
  -- ^ Whether this is the type of a signature, where what is applied is a
  -- type.
  }

-- | A walk over one line. Strict in the scan, which is small and wanted
-- whole at every step.
newtype Lex a = Lex {runLex :: Scan -> (a, Scan)}

instance Functor Lex where
  fmap f (Lex g) = Lex (\s -> case g s of (a, s') -> (f a, s'))
  {-# INLINE fmap #-}

instance Applicative Lex where
  pure a = Lex (\s -> (a, s))
  {-# INLINE pure #-}
  Lex f <*> Lex g = Lex (\s -> case f s of (h, s') -> case g s' of (a, s'') -> (h a, s''))
  {-# INLINE (<*>) #-}

instance Monad Lex where
  Lex g >>= f = Lex (\s -> case g s of (a, s') -> runLex (f a) s')
  {-# INLINE (>>=) #-}

-- | The scan as it stands: what is left of the line, and what the tokens
-- before it were. Every branch below decides on this and nothing else.
scan :: Lex Scan
scan = Lex (\s -> (s, s))
{-# INLINE scan #-}

-- | Takes the next @n@ characters as a span of this kind. Runs of one kind
-- join, so a token may be taken in pieces.
eat :: Int -> TokenKind -> Lex ()
eat n k = Lex (\s -> ((), s {scanRest = T.drop n (scanRest s), scanSpans = push n k (scanSpans s)}))
{-# INLINE eat #-}

-- | Takes the characters this holds for.
eatWhile :: (Char -> Bool) -> TokenKind -> Lex ()
eatWhile p k = do
  t <- scanRest <$> scan
  eat (T.length (T.takeWhile p t)) k
{-# INLINE eatWhile #-}

-- | Takes all that is left of the line.
eatRest :: TokenKind -> Lex ()
eatRest k = do
  t <- scanRest <$> scan
  eat (T.length t) k
{-# INLINE eatRest #-}

-- | Takes up to and including the first of the delimiters, and says which it
-- was; or, if none of them is there, takes the rest of the line. The one that
-- starts earliest wins, and of those that tie the first given.
eatTo :: [(Text, a)] -> TokenKind -> Lex (Maybe a)
eatTo ds k = do
  t <- scanRest <$> scan
  case foldl' (nearer t) Nothing ds of
    Nothing -> Nothing <$ eat (T.length t) k
    Just (start, d, a) -> Just a <$ eat (start + T.length d) k
  where
    nearer t best (d, a) = case T.breakOn d t of
      (pre, post)
        | T.null post -> best
        | Just (start, _, _) <- best, start <= T.length pre -> best
        | otherwise -> Just (T.length pre, d, a)

-- | Says whether the token just taken was an operand.
operand :: Bool -> Lex ()
operand p = Lex (\s -> ((), s {scanPrev = p}))
{-# INLINE operand #-}

-- | Says whether the rest of the line is the type of a signature.
signature :: Bool -> Lex ()
signature g = Lex (\s -> ((), s {scanSig = g}))
{-# INLINE signature #-}

push :: Int -> TokenKind -> [Span] -> [Span]
push 0 _ acc = acc
push n k (Span m k' : acc) | k == k' = Span (n + m) k : acc
push n k acc = Span n k : acc

-- | The spans of a line and the state the next line starts in.
lexLine :: Lang -> LexState -> Text -> ([Span], LexState)
lexLine lang st0 line
  -- An empty line has nothing to colour and leaves the state as it was.
  | T.null line = ([], st0)
  | langHeadings lang && st0 == LexNormal && T.isPrefixOf "#" line =
      ([Span (T.length line) TokKeyword], LexNormal)
  -- No gap after all: the line is code, and a line of its own. Only the start
  -- of a line is ever lexed in that state.
  | LexGap _ <- st0
  , Just (c, _) <- T.uncons (T.stripStart line)
  , c /= '\\' =
      lexLine lang LexNormal line
  | otherwise = case runLex (start st0) (Scan line [] False sig0) of
      (st, end) -> (reverse (scanSpans end), st)
  where
    start = \case
      LexNormal -> normal
      LexBlock depth -> block depth
      LexString delim -> multiString delim
      LexGap q -> gap q

    normal :: Lex LexState
    normal = do
      Scan t spans prev sig <- scan
      case T.uncons t of
        Nothing -> pure LexNormal
        Just (c, after1)
          | isSpace c -> eatWhile isSpace TokPlain >> normal
          -- Before the line comments: Lua's block comment opens with its
          -- line comment's marker.
          | Just (open, _) <- langBlockComment lang
          , open `T.isPrefixOf` t ->
              eat (T.length open) TokComment >> block 1
          | any (`T.isPrefixOf` t) (langLineComments lang) ->
              eatRest TokComment >> pure LexNormal
          | Just delim <- firstPrefix (langMultiStrings lang) t ->
              eat (T.length delim) TokString >> multiString delim
          | c `elem` langStrings lang -> string c
          | c == '\'' && langCharLiterals lang ->
              case charLiteralLength t of
                Just n -> eat n TokString >> operand True >> normal
                Nothing -> eat 1 TokPlain >> normal
          | isDigit c ->
              eatWhile (\x -> isAlphaNum x || x == '.' || x == '_') TokNumber
                >> operand True
                >> normal
          | c == '#' && langDirectives lang ->
              -- The mark and the word after it are the one keyword.
              eat 1 TokKeyword >> eatWhile isAlpha TokKeyword >> operand False >> normal
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
                    | langApplication lang && (lineStart spans || not prev && not sig && applied after) = TokFunction
                    | otherwise = TokPlain
                  -- A qualifier is part of the name after it, which is an
                  -- operand or not as it would be alone.
                  prev' = case kind of
                    TokKeyword -> False
                    TokModule -> prev
                    _ -> True
               in eat n kind >> operand prev' >> normal
          | c == '.'
          , Span _ TokModule : _ <- spans ->
              eat 1 TokModule >> normal
          | otherwise ->
              -- A closing bracket ends an operand, and what follows a lambda's
              -- backslash are its parameters.
              let prev' = c `elem` [')', ']', '}', '\\']
                  sig' = sig || langApplication lang && c == ':' && T.isPrefixOf ":" after1
               in eat 1 TokPunct >> operand prev' >> signature sig' >> normal

    -- A block comment, which the language may let nest. One the language has
    -- no delimiters for cannot have been opened, so it ends with the line.
    block :: Int -> Lex LexState
    block depth = case langBlockComment lang of
      Nothing -> eatRest TokComment >> pure LexNormal
      Just (open, close) -> do
        hit <- eatTo ((close, False) : [(open, True) | langNestedComments lang]) TokComment
        case hit of
          Nothing -> pure (LexBlock depth)
          Just True -> block (depth + 1)
          Just False
            | depth <= 1 -> normal
            | otherwise -> block (depth - 1)

    -- A string that runs over lines, to its delimiter or to the line's end.
    multiString :: Text -> Lex LexState
    multiString delim = do
      hit <- eatTo [(delim, ())] TokString
      case hit of
        Nothing -> pure (LexString delim)
        Just () -> operand True >> normal

    -- A gap is any white space, blank lines among it. What follows it on this
    -- line, if anything, is the backslash that resumes the string.
    gap :: Char -> Lex LexState
    gap q = do
      eatWhile isSpace TokPlain
      t <- scanRest <$> scan
      if T.null t then pure (LexGap q) else string q

    -- A string at the scan: the character that opens or resumes it, and then
    -- its body.
    string :: Char -> Lex LexState
    string q = do
      t <- scanRest <$> scan
      let (m, broken) = stringLength q (T.drop 1 t)
      eat (1 + m) TokString
      if broken && langStringGaps lang
        then pure (LexGap q)
        else operand True >> normal

    -- Length of a string's body and closing quote, escapes skipped over, and
    -- whether a backslash ending the line broke it off: one with nothing
    -- after it but white space, which is the gap's. A string left open ends
    -- with the line.
    stringLength q = go 0
      where
        go !n t = case T.uncons t of
          Nothing -> (n, False)
          Just ('\\', r)
            | T.all isSpace r -> (n + 1 + T.length r, True)
            | otherwise -> go (n + 2) (T.drop 1 r)
          Just (x, r)
            | x == q -> (n + 1, False)
            | otherwise -> go (n + 1) r

    charLiteralLength t = case T.unpack (T.take 4 t) of
      ['\'', x, '\'', _] | x /= '\\' -> Just 3
      ['\'', x, '\''] | x /= '\\' -> Just 3
      -- The character after the backslash is the escape's, whatever it is, so
      -- the closing quote is looked for past it.
      '\'' : '\\' : _ ->
        let body = T.takeWhile (/= '\'') (T.drop 3 t)
            n = 3 + T.length body + 1
         in if n <= 12 && T.length t >= n then Just n else Nothing
      _ -> Nothing

    -- A line that carries on a signature starts inside its type.
    sig0 = langApplication lang && any (`T.isPrefixOf` T.stripStart line) ["->", "=>", "::"]

    lineStart spans = null spans && st0 == LexNormal

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

-- | The state a line leaves behind, without its spans.
lexState :: Lang -> LexState -> Text -> LexState
lexState lang st line = snd (lexLine lang st line)
