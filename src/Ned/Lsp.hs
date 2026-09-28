-- | Language servers, as much of them as going to a definition and asking
-- what is under the caret needs.
--
-- A server is a command run in a shell, spoken to in JSON-RPC over its
-- standard input and output. It is started the first time a file of its
-- language asks for it, and kept for as long as the window is open. What it
-- is told of a file is the whole text, again each time it has changed since
-- it was last told, which is all the syncing there is. What it finds wrong
-- in a file it sends when it likes, and that is handed to a function given
-- when it was started.
--
-- Everything here blocks until the server answers, so it is for a thread of
-- its own. Positions are the protocol's: a line counted from zero, and a
-- column in UTF-16 code units, which 'toUtf16' and 'fromUtf16' change to and
-- from characters of the line.
module Ned.Lsp
  ( Servers
  , newServers
  , Server
  , serverFor
  , didNotStart
  , Diagnostic (..)
  , severityName
  , syncDoc
  , definition
  , hover
  , languageId
  , toUtf16
  , fromUtf16

    -- * A thread for the latest
  , Latest
  , newLatest
  , handLatest
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forever, void, (<=<))
import Data.Aeson.Micro (Value (..), decodeStrict, encodeStrict, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, isAlphaNum, ord)
import Data.IORef
import Data.Functor ((<&>))
import Data.Maybe (isNothing, mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Numeric (readHex, showHex)
import System.FilePath (isPathSeparator)
import System.IO
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)

-- | The servers started so far, by the command line and the folder they
-- were started in; 'Nothing' for one that would not start.
type Servers = MVar (Map ([String], FilePath) (Maybe Server))

newServers :: IO Servers
newServers = newMVar Map.empty

data Server = Server
  { srvIn :: !(MVar Handle)
  , srvNext :: !(IORef Int)
  , srvWaiting :: !(IORef (Maybe (Map Int (MVar (Either Text Value)))))
  -- ^ The requests not answered yet; 'Nothing' once the server has stopped.
  , srvDocs :: !(MVar (Map FilePath (Int, Text)))
  -- ^ Each file the server has been told of, at the version and text it
  -- was last told.
  , srvDiagnosed :: !(FilePath -> [Diagnostic] -> IO ())
  }

-- | Something a server finds wrong in a file: where, from and to, how bad
-- (1 an error, 2 a warning, 3 information, 4 a hint), and what.
data Diagnostic = Diagnostic
  { diagStart :: !(Int, Int)
  , diagEnd :: !(Int, Int)
  , diagSeverity :: !Int
  , diagMessage :: !Text
  }
  deriving (Eq, Show)

-- | What a diagnostic's severity is called.
severityName :: Int -> Text
severityName = \case
  1 -> "error"
  2 -> "warning"
  3 -> "info"
  4 -> "hint"
  _ -> "note"

-- | The server for a command line in a folder: the one running already, or
-- one started and initialized now, which hands what it finds wrong in a file
-- to @diagnosed@. One that has stopped, or would not start, is tried again.
serverFor :: Servers -> (FilePath -> [Diagnostic] -> IO ()) -> [String] -> FilePath -> IO Server
serverFor servers diagnosed cmd root = either throwIO pure <=< modifyMVar servers $ \running -> do
  let fresh =
        try @SomeException (start diagnosed cmd root) <&> \r ->
          (Map.insert (cmd, root) (either (const Nothing) Just r) running, r)
  case Map.lookup (cmd, root) running of
    Just (Just s) ->
      readIORef (srvWaiting s) >>= \case
        Just _ -> pure (running, Right s)
        Nothing -> fresh
    _ -> fresh

-- | Whether the server for a command line in a folder was tried and would
-- not start, or has stopped since: what keeps a server that is not there
-- from being tried again at every key.
didNotStart :: Servers -> [String] -> FilePath -> IO Bool
didNotStart servers cmd root =
  withMVar servers $ \running -> case Map.lookup (cmd, root) running of
    Nothing -> pure False
    Just Nothing -> pure True
    Just (Just s) -> isNothing <$> readIORef (srvWaiting s)

start :: (FilePath -> [Diagnostic] -> IO ()) -> [String] -> FilePath -> IO Server
start diagnosed cmd root = do
  (prog, args) <- case cmd of
    p : as -> pure (p, as)
    [] -> ioError (userError "the shell to run language servers in is empty")
  (Just hin, Just hout, _, _) <- createProcess (proc prog args) {cwd = Just root, std_in = CreatePipe, std_out = CreatePipe}
  mapM_ (`hSetBinaryMode` True) [hin, hout]
  s <- Server <$> newMVar hin <*> newIORef 0 <*> newIORef (Just Map.empty) <*> newMVar Map.empty <*> pure diagnosed
  -- When the output ends, whatever is still waiting is told so.
  void . forkIO $ do
    _ <- try @SomeException (forever (readMessage hout >>= answer s))
    waiting <- atomicModifyIORef' (srvWaiting s) (Nothing,)
    mapM_ (mapM_ (`putMVar` Left "the language server stopped")) waiting
  _ <-
    orFail
      =<< call s "initialize" (object ["processId" .= Null, "rootUri" .= pathUri root, "capabilities" .= capabilities])
  notify s "initialized" (object [])
  pure s

-- | What the client can take: of what it does not say, only hover's text is
-- worth saying, which is taken as markdown.
capabilities :: Value
capabilities = object ["textDocument" .= object ["hover" .= object ["contentFormat" .= (["markdown", "plaintext"] :: [Text])]]]

orFail :: Either Text a -> IO a
orFail = either (throwIO . userError . T.unpack) pure

-- | What came from the server: an answer to one of ours, a request of its
-- own, which is answered with nothing, or a notification, of which only the
-- diagnostics are listened to.
answer :: Server -> Value -> IO ()
answer s = \case
  Object m
    | Just (String "textDocument/publishDiagnostics") <- Map.lookup "method" m
    , Just (Object p) <- Map.lookup "params" m
    , Just (String uri) <- Map.lookup "uri" p
    , Just path <- uriPath uri ->
        srvDiagnosed s path $ case Map.lookup "diagnostics" p of
          Just (Array ds) -> mapMaybe diagnostic ds
          _ -> []
    | Just n <- Map.lookup "id" m, Just _ <- Map.lookup "method" m ->
        send s (object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "result" .= Null])
    | Just (Number n) <- Map.lookup "id" m -> do
        box <- atomicModifyIORef' (srvWaiting s) $ \case
          Nothing -> (Nothing, Nothing)
          Just w -> (Just (Map.delete (round n) w), Map.lookup (round n) w)
        mapM_ (`putMVar` reply m) box
  _ -> pure ()
  where
    diagnostic = \case
      Object d
        | Just (Object r) <- Map.lookup "range" d
        , Just from <- position =<< Map.lookup "start" r
        , Just to <- position =<< Map.lookup "end" r
        , Just (String msg) <- Map.lookup "message" d ->
            Just (Diagnostic from to (case Map.lookup "severity" d of Just (Number n) -> round n; _ -> 1) msg)
      _ -> Nothing
    reply m = case Map.lookup "error" m of
      Just (Object e) | Just (String msg) <- Map.lookup "message" e -> Left msg
      Just _ -> Left "the language server answered with an error"
      Nothing -> Right (Map.findWithDefault Null "result" m)

call :: Server -> Text -> Value -> IO (Either Text Value)
call s method params = do
  n <- atomicModifyIORef' (srvNext s) (\i -> (i + 1, i))
  box <- newEmptyMVar
  live <- atomicModifyIORef' (srvWaiting s) $ \case
    Nothing -> (Nothing, False)
    Just w -> (Just (Map.insert n box w), True)
  if not live
    then pure (Left "the language server stopped")
    else do
      send s (object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= method, "params" .= params])
      takeMVar box

notify :: Server -> Text -> Value -> IO ()
notify s method params = send s (object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params])

send :: Server -> Value -> IO ()
send s msg = withMVar (srvIn s) $ \h -> do
  let body = encodeStrict msg
  BS.hPut h (BC.pack ("Content-Length: " <> show (BS.length body) <> "\r\n\r\n") <> body)
  hFlush h

-- | One message: the headers, of which only its length is read, a blank
-- line, and that many bytes of JSON.
readMessage :: Handle -> IO Value
readMessage h = headers Nothing
  where
    headers len = do
      l <- BC.filter (/= '\r') <$> BC.hGetLine h
      case BC.break (== ':') l of
        ("", _) -> maybe (headers Nothing) body len
        (name, rest)
          | BC.map toLowerAscii name == "content-length", Just (n, _) <- BC.readInt (BC.dropWhile (== ' ') (BS.drop 1 rest)) -> headers (Just n)
          | otherwise -> headers len
    body n = maybe (ioError (userError "the language server sent something that is not JSON")) pure . decodeStrict =<< BS.hGet h n
    toLowerAscii c = if c >= 'A' && c <= 'Z' then chr (ord c + 32) else c

-- | Tell the server what a file holds now, when it has not been told yet.
syncDoc :: Server -> FilePath -> Text -> Text -> IO ()
syncDoc s path lang text = modifyMVar_ (srvDocs s) $ \docs -> case Map.lookup path docs of
  Nothing -> do
    notify s "textDocument/didOpen" $
      object ["textDocument" .= object ["uri" .= pathUri path, "languageId" .= lang, "version" .= (0 :: Int), "text" .= text]]
    pure (Map.insert path (0, text) docs)
  Just (v, old)
    | old /= text -> do
        notify s "textDocument/didChange" $
          object
            [ "textDocument" .= object ["uri" .= pathUri path, "version" .= (v + 1)]
            , "contentChanges" .= [object ["text" .= text]]
            ]
        pure (Map.insert path (v + 1, text) docs)
    | otherwise -> pure docs

-- | Where what is at a place in a file is defined: the first of the places
-- the server gives, when it gives any.
definition :: Server -> FilePath -> (Int, Int) -> IO (Maybe (FilePath, (Int, Int)))
definition s path pos = located <$> (orFail =<< call s "textDocument/definition" (atPosition path pos))
  where
    located = \case
      Array (v : _) -> located v
      Object m
        | Just (String uri) <- Map.lookup "uri" m <|> Map.lookup "targetUri" m
        , Just (Object r) <- Map.lookup "range" m <|> Map.lookup "targetSelectionRange" m
        , Just p <- position =<< Map.lookup "start" r ->
            (,p) <$> uriPath uri
      _ -> Nothing

-- | What the server says of what is at a place in a file, as markdown.
hover :: Server -> FilePath -> (Int, Int) -> IO (Maybe Text)
hover s path pos = nonBlank . contents <$> (orFail =<< call s "textDocument/hover" (atPosition path pos))
  where
    contents = \case
      Object m | Just c <- Map.lookup "contents" m -> markup c
      _ -> ""
    markup = \case
      String t -> t
      Array vs -> T.intercalate "\n\n" (map markup vs)
      Object m
        | Just (String t) <- Map.lookup "value" m, Just (String lang) <- Map.lookup "language" m -> "```" <> lang <> "\n" <> t <> "\n```"
        | Just (String t) <- Map.lookup "value" m -> t
      _ -> ""
    nonBlank t = if T.null (T.strip t) then Nothing else Just t

-- | A line and a UTF-16 column, from the protocol's JSON.
position :: Value -> Maybe (Int, Int)
position = \case
  Object p | Just (Number l) <- Map.lookup "line" p, Just (Number c) <- Map.lookup "character" p -> Just (round l, round c)
  _ -> Nothing

atPosition :: FilePath -> (Int, Int) -> Value
atPosition path (l, c) =
  object ["textDocument" .= object ["uri" .= pathUri path], "position" .= object ["line" .= l, "character" .= c]]

-- | The protocol's name for a language, from the one the status bar shows.
languageId :: Text -> Text
languageId = \case
  "C++" -> "cpp"
  "C#" -> "csharp"
  "Shell" -> "shellscript"
  name -> T.toLower name

-- | The UTF-16 column of a character of a line, and back.
toUtf16 :: Text -> Int -> Int
toUtf16 l c = T.foldl' (\n ch -> n + if ord ch > 0xFFFF then 2 else 1) 0 (T.take c l)

fromUtf16 :: Text -> Int -> Int
fromUtf16 l u = length (takeWhile (<= u) (scanl1 (+) [if ord ch > 0xFFFF then 2 else 1 | ch <- T.unpack l]))

-- | A file's URI, and the file a URI names, if it names one.
pathUri :: FilePath -> Text
pathUri path = "file://" <> T.pack (concatMap escape (BS.unpack (T.encodeUtf8 (T.pack slashed))))
  where
    slashed = (if take 1 path == "/" then "" else "/") <> map (\c -> if isPathSeparator c then '/' else c) path
    escape w
      | isAlphaNum c && w < 128 || c `elem` ("/-._~:" :: String) = [c]
      | otherwise = '%' : (if w < 16 then "0" else "") <> showHex w ""
      where
        c = chr (fromIntegral w)

uriPath :: Text -> Maybe FilePath
uriPath uri = do
  rest <- T.stripPrefix "file://" uri
  let path = T.unpack (T.decodeUtf8Lenient (BS.pack (unescape (T.unpack rest))))
  -- file:///C:/x is C:/x on Windows.
  pure $ case path of
    '/' : d : ':' : more -> d : ':' : more
    _ -> path
  where
    unescape = \case
      '%' : a : b : more | [(w, "")] <- readHex [a, b] -> w : unescape more
      c : more -> BS.unpack (T.encodeUtf8 (T.singleton c)) <> unescape more
      [] -> []

--------------------------------------------------------------------------------
-- A thread for the latest
--------------------------------------------------------------------------------

-- | A thread that runs what it is handed, one at a time, and of what is
-- handed to it while it is busy only the last: what tells a server of a file
-- as it is typed into, where only the text as it stands matters.
data Latest = Latest !(IORef (Maybe (IO ()))) !(MVar ())

newLatest :: IO Latest
newLatest = do
  l@(Latest next ready) <- Latest <$> newIORef Nothing <*> newEmptyMVar
  void . forkIO . forever $ do
    takeMVar ready
    atomicModifyIORef' next (Nothing,) >>= mapM_ (void . try @SomeException)
  pure l

handLatest :: Latest -> IO () -> IO ()
handLatest (Latest next ready) job = writeIORef next (Just job) >> void (tryPutMVar ready ())
