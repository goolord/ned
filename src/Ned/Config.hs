-- | The settings, read from a Dhall file.
--
-- The file is @config.dhall@ in ned's folder of the user's configuration
-- (@~/.config/ned@ on Linux, @%APPDATA%\\ned@ on Windows). It is laid over
-- 'defaultConfigText', so it only has to hold what it changes:
--
-- > { bufferFontSize = 17.0, vimKeys = False }
--
-- A field the defaults do not have, or one of the wrong type, is an error,
-- so a misspelt setting is not passed over in silence.
--
-- Most settings are the window's. The rest, in 'FileSettings', are how a
-- file is worked on, and a project can set those for the files under its
-- root: a project is a record of them laid over the settings as the file is
-- laid over the defaults, and a project inside another is laid over the
-- outer one's. Whatever is added to 'FileSettings' can be set per project
-- with nothing more said. A file that does not
-- read is said so, and what is done then is the caller's: the window that is
-- opening starts on the defaults, and the window that is open keeps what it
-- has. 'watchConfig' reads the file again each time it changes.
module Ned.Config
  ( Config (..)
  , FileSettings (..)
  , Project (..)
  , settingsFor
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
import Control.Monad (unless, when)
import Data.Either.Validation (Validation (..))
import Data.List (isPrefixOf, sortOn)
import qualified Data.Set as Set
import Data.Ord (Down (..))
import Data.Traversable (for)
import qualified Dhall.Map as DM
import Dhall.Src (Src)
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Void (Void, absurd)
import Dhall (Decoder)
import qualified Dhall as D
import Dhall.Core (Chunks (..), Expr (Prefer, Record, RecordLit, TextLit), PreferAnnotation (..), RecordField (..), normalize)
import Lens.Micro (set)
import Ned.Editor.Types (clampFontSize, defaultFontSize)
import Ned.Text (clamp)
import Numeric.Natural (Natural)
import System.Directory (XdgDirectory (..), doesFileExist, getHomeDirectory, getModificationTime, getXdgDirectory)
import System.FilePath (normalise, splitDirectories, takeDirectory, takeExtension, (</>))
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
  , cfgFiles :: !FileSettings
  -- ^ How a file is worked on, where no project says otherwise.
  , cfgProjects :: ![Project]
  }
  deriving (Eq, Show)

-- | The settings that are a file's rather than the window's.
data FileSettings = FileSettings
  { fsServerShell :: ![Text]
  -- ^ The program a language server's command is run by, and the arguments
  -- that come before the command.
  , fsLanguageServers :: ![(Text, Text)]
  -- ^ A language's name, as the status bar has it, and its server's command.
  }
  deriving (Eq, Show)

-- | A folder whose files have settings of their own.
data Project = Project
  { projName :: !Text
  -- ^ Its field in @projects@.
  , projRoot :: !FilePath
  -- ^ Absolute.
  , projFiles :: !FileSettings
  -- ^ Its settings as they come out: the file's, then those of each project
  -- around this one, outermost first, then its own.
  }
  deriving (Eq, Show)

-- | The settings for a file, and the project it is in: the deepest of those
-- whose root holds it.
settingsFor :: Config -> FilePath -> (FileSettings, Maybe Project)
settingsFor cfg path =
  case sortOn (Down . depth . projRoot) [p | p <- cfgProjects cfg, projRoot p `holds` path] of
    p : _ -> (projFiles p, Just p)
    [] -> (cfgFiles cfg, Nothing)

-- | Whether a folder is, or is inside, another.
holds :: FilePath -> FilePath -> Bool
holds root path = splitDirectories root `isPrefixOf` splitDirectories path

depth :: FilePath -> Int
depth = length . splitDirectories

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
    , "  -- The window's size when it first opens, before any scale. After that it"
    , "  -- opens as it was closed, as does the file tree."
    , ", windowWidth = 1100"
    , ", windowHeight = 760"
    , "  -- Start in vim's normal mode; the View menu turns it off and on."
    , ", vimKeys = True"
    , ", showFileTree = True"
    , "  -- A dot for each space and a rule for each tab of a line's indentation."
    , ", showIndentation = True"
    , "  -- What a language server's command is run in: the program, and the"
    , "  -- arguments before the command."
    , ", languageServerShell = " <> (if isWindows then "[ \"cmd\", \"/c\" ]" else "[ \"sh\", \"-c\" ]")
    , "  -- A server for a language, named as the status bar names it, as in"
    , "  -- { language = \"C\", command = \"clangd\" }."
    , "  -- Its diagnostics are underlined. Ctrl+] goes to a definition; with vim's"
    , "  -- keys, gd does too, K shows what is under the caret, and g] and g[ go to"
    , "  -- the next diagnostic and the one before."
    , ", languageServers ="
    , "  [ { language = \"Haskell\", command = \"haskell-language-server-wrapper --lsp\" } ]"
    , "  -- Settings for the files under a folder: languageServerShell and"
    , "  -- languageServers, laid over the ones above as this file is laid over"
    , "  -- these defaults. A root is absolute, under ~, or from this file's folder."
    , "  -- A project inside another is laid over the outer one's settings, and its"
    , "  -- servers run in its root."
    , "  -- { app = { root = \"~/src/app\", languageServerShell = [ \"nix\", \"develop\", \"-c\", \"sh\", \"-c\" ] } }"
    , ", projects = {=}"
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
    , cfgFiles =
        FileSettings
          { fsServerShell = if isWindows then ["cmd", "/c"] else ["sh", "-c"]
          , fsLanguageServers = [("Haskell", "haskell-language-server-wrapper --lsp")]
          }
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
      <*> fileFields
      -- Read on their own, by 'readProjects'.
      <*> pure []
  where
    float = realToFrac <$> D.double
    font = (\name -> if isFontFile name then FontFile name else FontFamily name) . T.unpack <$> D.strictText
    int = fromIntegral . min 100000 <$> (D.natural :: Decoder Natural)

-- | The fields of 'FileSettings', which sit among the window's at the top
-- of the file and make up the whole of a project.
fileFields :: D.RecordDecoder FileSettings
fileFields =
  FileSettings
    <$> D.field "languageServerShell" (D.list D.strictText)
    <*> D.field "languageServers" (D.list (D.record ((,) <$> D.field "language" D.strictText <*> D.field "command" D.strictText)))

-- | Their names.
fileKeys :: [Text]
fileKeys = case D.expected (D.record fileFields) of
  Success (Record m) -> DM.keys m
  _ -> []

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
      -- The projects are a record whose fields differ in type from one to
      -- the next, which no decoder is written for: they are taken out, and
      -- each read on its own over what the rest comes to.
      case normalize (Prefer Nothing PreferFromSource defaults user) of
        RecordLit top -> do
          let rest = DM.delete "projects" top
          cfg <- D.fromExpr configDecoder (absurd <$> RecordLit rest)
          projects <- readProjects (takeDirectory path) rest (recordFieldValue <$> DM.lookup "projects" top)
          pure cfg {cfgProjects = projects}
        _ -> ioError (userError "The settings are not a record")

-- | Each project laid over the file settings at the top of the file, and
-- over the projects around it, outermost first. A project's fields are
-- checked against the file settings' own names first, so that one that
-- belongs to the window is said to be so rather than misspelt.
readProjects :: FilePath -> DM.Map Text (RecordField Src Void) -> Maybe (Expr Src Void) -> IO [Project]
readProjects dir top = \case
  Nothing -> pure []
  Just (RecordLit ps) -> do
    raw <- for (DM.toList ps) $ \(name, field) -> case recordFieldValue field of
      RecordLit m | Just (TextLit (Chunks [] root)) <- recordFieldValue <$> DM.lookup "root" m -> do
        let own = DM.delete "root" m
            stray = filter (`notElem` fileKeys) (DM.keys own)
        unless (null stray) . ioError . userError $
          "projects." <> T.unpack name <> ": " <> T.unpack (T.intercalate ", " stray) <> " cannot be set for a project; "
            <> T.unpack (T.intercalate " and " fileKeys) <> " can"
        (name,,own) <$> rootPath (T.unpack root)
      _ -> ioError (userError ("projects." <> T.unpack name <> " needs a root, as in { root = \"~/src/app\", languageServerShell = [ \"sh\", \"-c\" ] }"))
    for raw $ \(name, root, _) -> do
      let around = [own | (_, r, own) <- sortOn (\(n, r, _) -> (depth r, n)) raw, r `holds` root]
          laid = foldl (\acc own -> Prefer Nothing PreferFromSource acc (RecordLit own)) (RecordLit (DM.restrictKeys top (Set.fromList fileKeys))) around
      try (D.fromExpr (D.record fileFields) (absurd <$> laid)) >>= \case
        Left (e :: SomeException) -> ioError (userError ("projects." <> T.unpack name <> ":\n" <> displayException e))
        Right fs -> pure (Project name root fs)
  Just _ -> ioError (userError "projects is a record of projects, as in { app = { root = \"~/src/app\", languageServerShell = [ \"sh\", \"-c\" ] } }")
  where
    rootPath = \case
      "~" -> getHomeDirectory
      '~' : '/' : rest -> (</> rest) <$> getHomeDirectory
      p -> pure (normalise (dir </> p))

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
