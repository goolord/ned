-- | The settings, read from a Dhall file.
--
-- The file is @config.dhall@ in ned's folder of the user's configuration
-- (@~/.config/ned@ on Linux, @%APPDATA%\\ned@ on Windows). It is laid over
-- 'defaultConfigText', so it only has to hold what it changes:
--
-- > { bufferFontSize = 17.0, vimKeys = False }
--
-- A field the defaults do not have, or one of the wrong type, is an error,
-- so a misspelt setting is not passed over in silence. A file that does not
-- read is said so, and what is done then is the caller's: the window that is
-- opening starts on the defaults, and the window that is open keeps what it
-- has. 'watchConfig' reads the file again each time it changes.
module Ned.Config
  ( Config (..)
  , Font (..)
  , defaultConfig
  , defaultConfigText
  , configPath
  , Reading
  , readConfig
  , watchConfig
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (absurd)
import Dhall (Decoder)
import qualified Dhall as D
import Dhall.Core (Expr (Prefer), PreferAnnotation (..))
import Lens.Micro (set)
import Ned.Editor.Types (clampFontSize, defaultFontSize)
import Ned.Text (clamp)
import Numeric.Natural (Natural)
import System.Directory (XdgDirectory (..), doesFileExist, getModificationTime, getXdgDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.Info (os)

data Config = Config
  { cfgUiFontSize :: !Float
  -- ^ The menus, the bars and the tree, in points.
  , cfgBufferFontSize :: !Float
  -- ^ The text and the finder, in points, and where resetting the zoom goes
  -- back to.
  , cfgScale :: !Float
  -- ^ The whole window's zoom, over the display's pixel density. Zero or
  -- less follows the display's own scale.
  , cfgUiFont :: !(Maybe Font)
  -- ^ 'Nothing' is nano-ui's own choice.
  , cfgBufferFont :: !(Maybe Font)
  , cfgWindowWidth :: !Int
  , cfgWindowHeight :: !Int
  , cfgVimKeys :: !Bool
  , cfgShowFileTree :: !Bool
  , cfgShowIndentation :: !Bool
  , cfgShell :: ![Text]
  -- ^ The program a language server's command is run by, and the arguments
  -- that come before the command.
  , cfgLanguageServers :: ![(Text, Text)]
  -- ^ A language's name, as the status bar has it, and its server's command.
  , cfgProjects :: ![(FilePath, [(Text, Text)])]
  -- ^ Servers for the files under a folder, over the ones above.
  }
  deriving (Eq, Show)

-- | A font as the settings name it: by the file it is in, when the name ends
-- in a font file's extension, and otherwise by its family.
data Font = FontFile FilePath | FontFamily String
  deriving (Eq, Show)

-- | The defaults, as a Dhall record: what a file is laid over, and what
-- @ned --default-config@ prints to start one from. 'defaultConfig' is this
-- read back, and the tests hold the two to each other.
defaultConfigText :: Text
defaultConfigText =
  T.unlines
    [ "-- ned's settings. Keep the fields you change; the rest take these values."
    , "{ -- The menus, the bars and the tree, in points."
    , "  uiFontSize = 16.0"
    , "  -- The text and the finder, in points. Ctrl+0 resets the zoom to this."
    , ", bufferFontSize = 15.0"
    , "  -- The whole window's zoom, over the display's pixel density: 1.5 makes"
    , "  -- everything half as big again. 0.0 follows the display's own scale."
    , ", scale = 1.0"
    , "  -- A font family, as in Some \"Inter\", or a path to a .ttf or .otf file."
    , "  -- None Text keeps the default."
    , ", uiFont = None Text"
    , "  -- The same, for the text. Pick a monospaced font."
    , ", bufferFont = None Text"
    , "  -- The window's size when it opens, before any scale."
    , ", windowWidth = 1100"
    , ", windowHeight = 760"
    , "  -- Start in vim's normal mode; the View menu turns it off and on."
    , ", vimKeys = True"
    , ", showFileTree = True"
    , "  -- A dot for each space and a rule for each tab of a line's indentation."
    , ", showIndentation = True"
    , "  -- What a language server's command is run in: the program, and the"
    , "  -- arguments before the command."
    , ", shell = " <> (if isWindows then "[ \"cmd\", \"/c\" ]" else "[ \"sh\", \"-c\" ]")
    , "  -- A server for a language, named as the status bar names it:"
    , "  -- [ { language = \"Haskell\", command = \"haskell-language-server-wrapper --lsp\" } ]"
    , "  -- Ctrl+] goes to a definition; with vim's keys, gd does too, and K shows"
    , "  -- what is under the caret."
    , ", languageServers = [] : List { language : Text, command : Text }"
    , "  -- Servers for the files under a folder, in place of the ones above:"
    , "  -- [ { root = \"~/src/app\", languageServers = [ { language = \"Haskell\", command = \"nix develop -c haskell-language-server-wrapper --lsp\" } ] } ]"
    , ", projects = [] : List { root : Text, languageServers : List { language : Text, command : Text } }"
    , "}"
    ]

-- | The defaults, which are the settings when there is no file.
defaultConfig :: Config
defaultConfig =
  Config
    { cfgUiFontSize = 16
    , cfgBufferFontSize = defaultFontSize
    , cfgScale = 1
    , cfgUiFont = Nothing
    , cfgBufferFont = Nothing
    , cfgWindowWidth = 1100
    , cfgWindowHeight = 760
    , cfgVimKeys = True
    , cfgShowFileTree = True
    , cfgShowIndentation = True
    , cfgShell = if isWindows then ["cmd", "/c"] else ["sh", "-c"]
    , cfgLanguageServers = []
    , cfgProjects = []
    }

configDecoder :: Decoder Config
configDecoder =
  D.record $
    Config
      <$> D.field "uiFontSize" float
      <*> D.field "bufferFontSize" float
      <*> D.field "scale" float
      <*> D.field "uiFont" (D.maybe font)
      <*> D.field "bufferFont" (D.maybe font)
      <*> D.field "windowWidth" int
      <*> D.field "windowHeight" int
      <*> D.field "vimKeys" D.bool
      <*> D.field "showFileTree" D.bool
      <*> D.field "showIndentation" D.bool
      <*> D.field "shell" (D.list D.strictText)
      <*> D.field "languageServers" servers
      <*> D.field "projects" (D.list (D.record ((,) <$> D.field "root" D.string <*> D.field "languageServers" servers)))
  where
    servers = D.list (D.record ((,) <$> D.field "language" D.strictText <*> D.field "command" D.strictText))
    float = realToFrac <$> D.double
    font = (\name -> if isFontFile name then FontFile name else FontFamily name) . T.unpack <$> D.strictText
    int = fromIntegral . min 100000 <$> (D.natural :: Decoder Natural)

-- | Where the settings are read from.
configPath :: IO FilePath
configPath = (</> "config.dhall") <$> getXdgDirectory XdgConfig "ned"

-- | What reading the file came to: why it would not read, or the settings in
-- it and what in them could not be used.
type Reading = Either String (Config, Maybe String)

-- | The settings in a file laid over the defaults. No file is no fault: it
-- is the defaults.
--
-- The file is read on its own before it meets the defaults, so that what is
-- wrong in it is shown in its own lines, and what is wrong with the two
-- together is shown as the fields that differ.
readConfig :: FilePath -> IO Reading
readConfig path =
  doesFileExist path >>= \case
    False -> pure (Right (defaultConfig, Nothing))
    True ->
      try readMerged >>= \case
        Left (e :: SomeException) -> pure (Left (path <> ":\n" <> displayException e))
        Right cfg -> Right <$> checkFonts (bounded cfg)
  where
    readMerged = do
      -- Its imports are found beside it, and its errors say where it is.
      let settings = set D.rootDirectory (takeDirectory path) (set D.sourceName path D.defaultInputSettings)
      user <- D.inputExprWithSettings settings . T.decodeUtf8Lenient =<< BS.readFile path
      defaults <- D.inputExpr defaultConfigText
      D.fromExpr configDecoder (absurd <$> Prefer Nothing PreferFromSource defaults user)

-- | Read the file again each time it changes, and hand over each reading;
-- this never returns. The file's time is looked at twice a second, which is
-- a stat and asks nothing of the platform's own file watching. A file that
-- goes away is a change too, back to the defaults.
watchConfig :: FilePath -> (Reading -> IO ()) -> IO ()
watchConfig path found = stamp >>= go
  where
    stamp = either (const Nothing) Just <$> try @IOException (getModificationTime path)
    go seen = do
      threadDelay 500000
      now <- stamp
      when (now /= seen) (readConfig path >>= found)
      go now

-- | The sizes kept to what the window can be drawn at: the text to the zoom's
-- own range, and the window to something that is still a window.
bounded :: Config -> Config
bounded cfg =
  cfg
    { cfgUiFontSize = clamp 6 48 (cfgUiFontSize cfg)
    , cfgBufferFontSize = clampFontSize (cfgBufferFontSize cfg)
    , cfgScale = if cfgScale cfg <= 0 then 0 else clamp 0.25 8 (cfgScale cfg)
    , cfgWindowWidth = max 320 (cfgWindowWidth cfg)
    , cfgWindowHeight = max 240 (cfgWindowHeight cfg)
    }

-- | A font given as a file that is not there is dropped for the default,
-- since the window cannot open without its fonts. A family that is not
-- installed needs no check: the search falls back by itself.
checkFonts :: Config -> IO (Config, Maybe String)
checkFonts cfg = do
  (ui, uiErr) <- check (cfgUiFont cfg)
  (buf, bufErr) <- check (cfgBufferFont cfg)
  let errs = [e | Just e <- [uiErr, bufErr]]
  pure (cfg {cfgUiFont = ui, cfgBufferFont = buf}, if null errs then Nothing else Just (unlines errs))
  where
    check = \case
      Just (FontFile path) ->
        doesFileExist path >>= \case
          True -> pure (Just (FontFile path), Nothing)
          False -> pure (Nothing, Just ("No font file at " <> path <> "; using the default"))
      font -> pure (font, Nothing)

isWindows :: Bool
isWindows = os == "mingw32"

-- | Whether a font is named by its file rather than by its family.
isFontFile :: FilePath -> Bool
isFontFile path = map toLower (takeExtension path) `elem` [".ttf", ".otf", ".ttc", ".otc"]
