-- | The languages the editor knows, and how a file is matched to one.
--
-- Each is a 'Lang' built from 'plainText' or 'cLike' by saying what it does
-- differently, so that what a language has in common with its neighbours is
-- written once and what is peculiar to it is what you read here.
module Ned.Highlight.Languages
  ( plainText
  , languageFor
  ) where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Ned.Highlight.Lang
import System.FilePath (takeExtension, takeFileName)

plainText :: Lang
plainText =
  Lang
    { langName = "Plain Text"
    , langLineComments = []
    , langBlockComment = Nothing
    , langNestedComments = False
    , langStrings = []
    , langMultiStrings = []
    , langStringGaps = False
    , langCharLiterals = False
    , langKeywords = Set.empty
    , langTypes = Set.empty
    , langCapitalTypes = False
    , langCalls = False
    , langApplication = False
    , langQualifiers = False
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

-- | The language of a file, going by its extension, or failing that by its
-- whole name.
languageFor :: FilePath -> Lang
languageFor path = fromMaybe plainText (known (takeExtension path) <|> known (takeFileName path))
  where
    known key = Map.lookup (map toLower key) languages

-- | The languages, by the extensions and the file names they go by.
languages :: Map String Lang
languages =
  Map.fromList
    [ (key, lang)
    | (keys, lang) <-
        [ (".hs .lhs .hsc", haskell)
        , (".cabal .project", cabal)
        , (".c .h", cLang)
        , (".cc .cpp .cxx .hpp .hh", cpp)
        , (".rs", rust)
        , (".go", golang)
        , (".java", java)
        , (".cs", csharp)
        , (".js .jsx .mjs", javascript)
        , (".ts .tsx", typescript)
        , (".json", json)
        , (".py", python)
        , (".lua", lua)
        , (".zig", zig)
        , (".sh .bash .zsh", shell)
        , (".ps1", shell {langName = "PowerShell"})
        , (".nix", nix)
        , (".toml", toml)
        , (".yaml .yml", yaml)
        , (".md .markdown", markdown)
        , (".sql", sql)
        , (".css", css)
        , (".html .htm", html)
        , (".xml .svg", html {langName = "XML"})
        , ("makefile", shell {langName = "Makefile"})
        , ("dockerfile", shell {langName = "Dockerfile"})
        ]
    , key <- words keys
    ]

haskell :: Lang
haskell =
  plainText
    { langName = "Haskell"
    , langLineComments = ["--"]
    , langBlockComment = Just ("{-", "-}")
    , langNestedComments = True
    , langStrings = ['"']
    , langStringGaps = True
    , langCharLiterals = True
    , langCapitalTypes = True
    , langApplication = True
    , langQualifiers = True
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
    , langMultiStrings = [quoted "`"]
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
    , langMultiStrings = [quoted "`"]
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
    , langMultiStrings = [quoted "\"\"\"", quoted "'''"]
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
    , langMultiStrings = [MultiString "[[" "]]"]
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
    , langMultiStrings = [quoted "''"]
    , langIdentExtra = ['-', '\'']
    , langKeywords = ws "assert else if in inherit let or rec then with true false null import"
    }

toml :: Lang
toml =
  plainText
    { langName = "TOML"
    , langLineComments = ["#"]
    , langStrings = ['"', '\'']
    , langMultiStrings = [quoted "\"\"\"", quoted "'''"]
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
    , langMultiStrings = [quoted "```"]
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
