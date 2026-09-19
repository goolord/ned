-- | Lexical syntax highlighting, a line at a time.
--
-- A language is described by a 'Lang': its comments, strings, keywords and a
-- few switches. 'lexLine' colours one line given the state the line before it
-- ended in (inside a block comment or a string that runs over lines), so the
-- view lexes the lines on screen and nothing else.
module Ned.Highlight
  ( TokenKind (..)
  , Span (..)
  , LexState (..)
  , Lang (..)
  , lexLine
  , lexState
  , plainText
  , languageFor
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeExtension, takeFileName)

data TokenKind
  = TokPlain
  | TokKeyword
  | TokType
  | TokFunction
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
  deriving (Eq, Show)

data Lang = Lang
  { langName :: !Text
  , langLineComments :: ![Text]
  , langBlockComment :: !(Maybe (Text, Text))
  , langNestedComments :: !Bool
  , langStrings :: ![Char]
  -- ^ Delimiters of strings that end with their line.
  , langMultiStrings :: ![Text]
  -- ^ Delimiters of strings that run over lines.
  , langCharLiterals :: !Bool
  -- ^ Whether @'x'@ is a character, where a lone @'@ may be something else.
  , langKeywords :: !(Set Text)
  , langTypes :: !(Set Text)
  , langCapitalTypes :: !Bool
  -- ^ Whether a capitalised identifier names a type.
  , langCalls :: !Bool
  -- ^ Whether an identifier before @(@ is a function.
  , langIdentExtra :: ![Char]
  -- ^ Characters of identifiers besides letters, digits and @_@.
  , langDirectives :: !Bool
  -- ^ Whether @#word@ is a keyword, as in C.
  , langHeadings :: !Bool
  -- ^ Whether a line starting with @#@ is a heading, as in Markdown.
  }

--------------------------------------------------------------------------------
-- Lexing
--------------------------------------------------------------------------------

-- | The spans of a line and the state the next line starts in.
lexLine :: Lang -> LexState -> Text -> ([Span], LexState)
lexLine lang st0 line
  | langHeadings lang && st0 == LexNormal && T.isPrefixOf "#" line = ([Span (T.length line) TokKeyword], LexNormal)
  | otherwise = go st0 line []
  where
    done acc st = (reverse acc, st)

    push 0 _ acc = acc
    push n k (Span m k' : acc) | k == k' = Span (n + m) k : acc
    push n k acc = Span n k : acc

    go st t acc
      | T.null t = done acc st
    go (LexBlock depth) t acc =
      case langBlockComment lang of
        Nothing -> done (push (T.length t) TokComment acc) LexNormal
        Just (open, close) ->
          let (preC, restC) = T.breakOn close t
              (preO, restO) = T.breakOn open t
           in if langNestedComments lang && not (T.null restO) && T.length preO < T.length preC
                then
                  let n = T.length preO + T.length open
                   in go (LexBlock (depth + 1)) (T.drop n t) (push n TokComment acc)
                else
                  if T.null restC
                    then done (push (T.length t) TokComment acc) (LexBlock depth)
                    else
                      let n = T.length preC + T.length close
                          st = if depth <= 1 then LexNormal else LexBlock (depth - 1)
                       in go st (T.drop n t) (push n TokComment acc)
    go (LexString delim) t acc =
      let (pre, rest) = T.breakOn delim t
       in if T.null rest
            then done (push (T.length t) TokString acc) (LexString delim)
            else
              let n = T.length pre + T.length delim
               in go LexNormal (T.drop n t) (push n TokString acc)
    go LexNormal t acc =
      case T.uncons t of
        Nothing -> done acc LexNormal
        Just (c, rest)
          | isSpace c ->
              let n = T.length (T.takeWhile isSpace t)
               in go LexNormal (T.drop n t) (push n TokPlain acc)
          -- Before the line comments: Lua's block comment opens with its
          -- line comment's marker.
          | Just (open, _) <- langBlockComment lang
          , open `T.isPrefixOf` t ->
              let n = T.length open
               in go (LexBlock 1) (T.drop n t) (push n TokComment acc)
          | any (`T.isPrefixOf` t) (langLineComments lang) ->
              done (push (T.length t) TokComment acc) LexNormal
          | Just delim <- firstPrefix (langMultiStrings lang) t ->
              let n = T.length delim
               in go (LexString delim) (T.drop n t) (push n TokString acc)
          | c `elem` langStrings lang ->
              let n = 1 + stringLength c rest
               in go LexNormal (T.drop n t) (push n TokString acc)
          | c == '\'' && langCharLiterals lang ->
              case charLiteralLength t of
                Just n -> go LexNormal (T.drop n t) (push n TokString acc)
                Nothing -> go LexNormal rest (push 1 TokPlain acc)
          | isDigit c ->
              let n = T.length (T.takeWhile (\x -> isAlphaNum x || x == '.' || x == '_') t)
               in go LexNormal (T.drop n t) (push n TokNumber acc)
          | c == '#' && langDirectives lang ->
              let n = 1 + T.length (T.takeWhile isAlpha rest)
               in go LexNormal (T.drop n t) (push n TokKeyword acc)
          | isAlpha c || c == '_' ->
              let word = T.takeWhile isIdent t
                  n = T.length word
                  after = T.drop n t
                  kind
                    | word `Set.member` langKeywords lang = TokKeyword
                    | word `Set.member` langTypes lang = TokType
                    | langCapitalTypes lang && isUpper c = TokType
                    | langCalls lang && T.isPrefixOf "(" (T.stripStart after) = TokFunction
                    | otherwise = TokPlain
               in go LexNormal after (push n kind acc)
          | otherwise -> go LexNormal rest (push 1 TokPunct acc)

    isIdent x = isAlphaNum x || x == '_' || x `elem` langIdentExtra lang

    firstPrefix ds t = case filter (`T.isPrefixOf` t) ds of
      d : _ -> Just d
      [] -> Nothing

    -- Length of a string's body and closing quote, escapes skipped over. A
    -- string left open ends with the line.
    stringLength q = scan 0
      where
        scan !n t = case T.uncons t of
          Nothing -> n
          Just ('\\', r) -> scan (n + 1 + min 1 (T.length (T.take 1 r))) (T.drop 1 r)
          Just (x, r)
            | x == q -> n + 1
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

--------------------------------------------------------------------------------
-- Languages
--------------------------------------------------------------------------------

plainText :: Lang
plainText =
  Lang
    { langName = "Plain Text"
    , langLineComments = []
    , langBlockComment = Nothing
    , langNestedComments = False
    , langStrings = []
    , langMultiStrings = []
    , langCharLiterals = False
    , langKeywords = Set.empty
    , langTypes = Set.empty
    , langCapitalTypes = False
    , langCalls = False
    , langIdentExtra = []
    , langDirectives = False
    , langHeadings = False
    }

cLike :: Lang
cLike =
  plainText
    { langLineComments = ["//"]
    , langBlockComment = Just ("/*", "*/")
    , langStrings = ['"']
    , langCharLiterals = True
    , langCalls = True
    }

ws :: Text -> Set Text
ws = Set.fromList . T.words

-- | The language of a file, going by its name.
languageFor :: FilePath -> Lang
languageFor path =
  case map toLower (takeExtension path) of
    ".hs" -> haskell
    ".lhs" -> haskell
    ".hsc" -> haskell
    ".cabal" -> cabal
    ".project" -> cabal
    ".c" -> cLang
    ".h" -> cLang
    ".cc" -> cpp
    ".cpp" -> cpp
    ".cxx" -> cpp
    ".hpp" -> cpp
    ".hh" -> cpp
    ".rs" -> rust
    ".go" -> golang
    ".java" -> java
    ".cs" -> csharp
    ".js" -> javascript
    ".jsx" -> javascript
    ".mjs" -> javascript
    ".ts" -> typescript
    ".tsx" -> typescript
    ".json" -> json
    ".py" -> python
    ".lua" -> lua
    ".zig" -> zig
    ".sh" -> shell
    ".bash" -> shell
    ".zsh" -> shell
    ".ps1" -> shell {langName = "PowerShell"}
    ".nix" -> nix
    ".toml" -> toml
    ".yaml" -> yaml
    ".yml" -> yaml
    ".md" -> markdown
    ".markdown" -> markdown
    ".sql" -> sql
    ".css" -> css
    ".html" -> html
    ".htm" -> html
    ".xml" -> html {langName = "XML"}
    ".svg" -> html {langName = "XML"}
    _ -> case map toLower (takeFileName path) of
      "makefile" -> shell {langName = "Makefile"}
      "dockerfile" -> shell {langName = "Dockerfile"}
      "cabal.project" -> cabal
      _ -> plainText

haskell :: Lang
haskell =
  plainText
    { langName = "Haskell"
    , langLineComments = ["--"]
    , langBlockComment = Just ("{-", "-}")
    , langNestedComments = True
    , langStrings = ['"']
    , langCharLiterals = True
    , langCapitalTypes = True
    , langIdentExtra = ['\'']
    , langKeywords =
        ws
          "case class data default deriving do else family forall foreign hiding if import in infix infixl \
          \infixr instance let mdo module newtype of pattern proc qualified rec then type where as via stock anyclass"
    }

cabal :: Lang
cabal =
  plainText
    { langName = "Cabal"
    , langLineComments = ["--"]
    , langIdentExtra = ['-']
    , langKeywords =
        ws
          "library executable test-suite benchmark common flag import if else elif source-repository \
          \source-repository-package package packages"
    }

cKeywords :: Text
cKeywords =
  "auto break case const continue default do else enum extern for goto if inline register restrict return \
  \sizeof static struct switch typedef union volatile while _Alignas _Alignof _Atomic _Generic _Noreturn \
  \_Static_assert _Thread_local NULL true false"

cTypes :: Text
cTypes =
  "void char short int long float double signed unsigned bool size_t ssize_t ptrdiff_t intptr_t uintptr_t \
  \int8_t int16_t int32_t int64_t uint8_t uint16_t uint32_t uint64_t FILE"

cLang :: Lang
cLang = cLike {langName = "C", langDirectives = True, langKeywords = ws cKeywords, langTypes = ws cTypes}

cpp :: Lang
cpp =
  cLang
    { langName = "C++"
    , langKeywords =
        ws cKeywords
          <> ws
            "alignas alignof and bitand bitor catch class co_await co_return co_yield concept const_cast \
            \consteval constexpr constinit decltype delete dynamic_cast explicit export friend mutable \
            \namespace new noexcept not nullptr operator or private protected public reinterpret_cast \
            \requires static_assert static_cast template this thread_local throw try typeid typename using \
            \virtual xor override final"
    , langTypes = ws cTypes <> ws "string wstring vector map set unordered_map unordered_set unique_ptr shared_ptr optional variant"
    }

rust :: Lang
rust =
  cLike
    { langName = "Rust"
    , langNestedComments = True
    , langCapitalTypes = True
    , langKeywords =
        ws
          "as async await break const continue crate dyn else enum extern false fn for if impl in let loop \
          \match mod move mut pub ref return self Self static struct super trait true type unsafe use where while"
    , langTypes = ws "i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f32 f64 bool char str"
    }

golang :: Lang
golang =
  cLike
    { langName = "Go"
    , langMultiStrings = ["`"]
    , langKeywords =
        ws
          "break case chan const continue default defer else fallthrough for func go goto if import \
          \interface map package range return select struct switch type var nil true false iota"
    , langTypes =
        ws
          "bool byte complex64 complex128 error float32 float64 int int8 int16 int32 int64 rune string \
          \uint uint8 uint16 uint32 uint64 uintptr any"
    }

java :: Lang
java =
  cLike
    { langName = "Java"
    , langCapitalTypes = True
    , langKeywords =
        ws
          "abstract assert break case catch class const continue default do else enum extends final finally \
          \for goto if implements import instanceof interface native new package private protected public \
          \return static strictfp super switch synchronized this throw throws transient try volatile while \
          \var record sealed permits yield null true false"
    , langTypes = ws "boolean byte char double float int long short void"
    }

csharp :: Lang
csharp =
  java
    { langName = "C#"
    , langDirectives = True
    , langKeywords =
        langKeywords java
          <> ws
            "as async await base checked delegate event explicit extern fixed foreach get implicit in internal \
            \is lock namespace operator out override params readonly ref sealed set sizeof stackalloc struct \
            \typeof unchecked unsafe using virtual where"
    , langTypes = langTypes java <> ws "bool decimal object sbyte string uint ulong ushort dynamic"
    }

javascript :: Lang
javascript =
  cLike
    { langName = "JavaScript"
    , langStrings = ['"', '\'']
    , langCharLiterals = False
    , langMultiStrings = ["`"]
    , langCapitalTypes = True
    , langIdentExtra = ['$']
    , langKeywords =
        ws
          "async await break case catch class const continue debugger default delete do else export extends \
          \finally for from function get if import in instanceof let new of return set static super switch \
          \this throw try typeof var void while with yield null undefined true false NaN Infinity"
    }

typescript :: Lang
typescript =
  javascript
    { langName = "TypeScript"
    , langKeywords =
        langKeywords javascript
          <> ws "abstract as declare enum implements interface is keyof namespace private protected public readonly satisfies type"
    , langTypes = ws "any boolean never number object string symbol unknown bigint"
    }

json :: Lang
json = plainText {langName = "JSON", langStrings = ['"'], langKeywords = ws "true false null"}

python :: Lang
python =
  plainText
    { langName = "Python"
    , langLineComments = ["#"]
    , langStrings = ['"', '\'']
    , langMultiStrings = ["\"\"\"", "'''"]
    , langCalls = True
    , langKeywords =
        ws
          "and as assert async await break case class continue def del elif else except finally for from \
          \global if import in is lambda match nonlocal not or pass raise return try while with yield \
          \None True False self"
    , langTypes = ws "bool bytes dict float frozenset int list object set str tuple type"
    }

lua :: Lang
lua =
  plainText
    { langName = "Lua"
    , langLineComments = ["--"]
    , langBlockComment = Just ("--[[", "]]")
    , langStrings = ['"', '\'']
    , langMultiStrings = ["[["]
    , langCalls = True
    , langKeywords =
        ws
          "and break do else elseif end false for function goto if in local nil not or repeat return then \
          \true until while"
    }

zig :: Lang
zig =
  plainText
    { langName = "Zig"
    , langLineComments = ["//"]
    , langStrings = ['"']
    , langCharLiterals = True
    , langCalls = True
    , langKeywords =
        ws
          "addrspace align allowzero and anyframe anytype asm async await break callconv catch comptime \
          \const continue defer else enum errdefer error export extern fn for if inline noalias nosuspend \
          \opaque or orelse packed pub resume return linksection struct suspend switch test threadlocal try \
          \union unreachable usingnamespace var volatile while null undefined true false"
    , langTypes =
        ws
          "i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f16 f32 f64 f80 f128 bool void noreturn type \
          \anyerror anyopaque comptime_int comptime_float"
    }

shell :: Lang
shell =
  plainText
    { langName = "Shell"
    , langLineComments = ["#"]
    , langStrings = ['"', '\'']
    , langIdentExtra = ['-']
    , langKeywords =
        ws
          "if then else elif fi for while until do done case esac in function select return exit export \
          \local readonly declare set unset source alias echo cd"
    }

nix :: Lang
nix =
  plainText
    { langName = "Nix"
    , langLineComments = ["#"]
    , langBlockComment = Just ("/*", "*/")
    , langStrings = ['"']
    , langMultiStrings = ["''"]
    , langIdentExtra = ['-', '\'']
    , langKeywords = ws "assert else if in inherit let or rec then with true false null import"
    }

toml :: Lang
toml =
  plainText
    { langName = "TOML"
    , langLineComments = ["#"]
    , langStrings = ['"', '\'']
    , langMultiStrings = ["\"\"\"", "'''"]
    , langIdentExtra = ['-']
    , langKeywords = ws "true false"
    }

yaml :: Lang
yaml =
  plainText
    { langName = "YAML"
    , langLineComments = ["#"]
    , langStrings = ['"', '\'']
    , langIdentExtra = ['-']
    , langKeywords = ws "true false null yes no on off"
    }

markdown :: Lang
markdown =
  plainText
    { langName = "Markdown"
    , langMultiStrings = ["```"]
    , langStrings = ['`']
    , langHeadings = True
    }

sql :: Lang
sql =
  plainText
    { langName = "SQL"
    , langLineComments = ["--"]
    , langBlockComment = Just ("/*", "*/")
    , langStrings = ['\'']
    , langCalls = True
    , langKeywords =
        let kw =
              "add all alter and as asc begin between by case check column commit constraint create cross \
              \default delete desc distinct drop else end exists foreign from full group having if in index \
              \inner insert into is join key left like limit not null offset on or order outer primary \
              \references right rollback select set table then union unique update values view when where with"
         in ws kw <> ws (T.toUpper kw)
    , langTypes =
        let ty = "bigint boolean char date decimal double float int integer numeric real smallint text time timestamp varchar"
         in ws ty <> ws (T.toUpper ty)
    }

css :: Lang
css =
  plainText
    { langName = "CSS"
    , langBlockComment = Just ("/*", "*/")
    , langStrings = ['"', '\'']
    , langIdentExtra = ['-']
    , langCalls = True
    }

html :: Lang
html =
  plainText
    { langName = "HTML"
    , langBlockComment = Just ("<!--", "-->")
    , langStrings = ['"', '\'']
    , langIdentExtra = ['-']
    }
