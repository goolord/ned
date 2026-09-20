-- | Files on disk: reading one into a buffer, and writing one back the way
-- it came.
--
-- A file is more than its text, and what is not text is kept here and put
-- back when it is saved: how it ended its lines, and whether it began with a
-- byte order mark. Nothing above this module has to think about either, and
-- a file that came in as CRLF with a mark goes out as one.
module Ned.File
  ( Eol (..)
  , FileFormat (..)
  , Loaded (..)
  , loadFile
  , saveFile
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.NanoRope as Rope
import Ned.Buffer (Buffer)
import qualified Ned.Buffer as B
import System.IO (IOMode (WriteMode), withBinaryFile)

-- | How the file on disk ends its lines. The buffer always holds @\\n@.
data Eol = LF | CRLF
  deriving (Eq, Show)

-- | What of a file is not its text, and is put back when it is saved.
data FileFormat = FileFormat
  { formatEol :: !Eol
  , formatBom :: !Bool
  -- ^ Whether the file starts with a UTF-8 byte order mark.
  }
  deriving (Eq, Show)

-- | A file as read.
data Loaded = Loaded
  { loadedBuffer :: !Buffer
  , loadedFormat :: !FileFormat
  , loadedLossy :: !Bool
  -- ^ Whether the file was not UTF-8, so that bytes of it became U+FFFD and
  -- saving will not bring them back.
  }

-- | Read a file as UTF-8, bytes that are not becoming U+FFFD, with its line
-- endings turned to @\\n@.
loadFile :: FilePath -> IO (Either Text Loaded)
loadFile path = do
  result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
  pure $ case result of
    Left e -> Left (T.pack (show e))
    Right raw ->
      let (bom, bytes) = maybe (False, raw) ((,) True) (BS.stripPrefix utf8Bom raw)
          (lossy, text) = case TE.decodeUtf8' bytes of
            Right t -> (False, t)
            Left _ -> (True, TE.decodeUtf8With (\_ _ -> Just '\xFFFD') bytes)
          eol = if "\r\n" `T.isInfixOf` T.take 65536 text then CRLF else LF
          unix = if eol == CRLF then T.replace "\r\n" "\n" text else text
       in Right (Loaded (B.fromText unix) (FileFormat eol bom) lossy)

utf8Bom :: BS.ByteString
utf8Bom = "\xEF\xBB\xBF"

-- | Write the rope out a chunk at a time, never as one text, in the format
-- the file came in.
saveFile :: FilePath -> FileFormat -> Buffer -> IO (Either Text ())
saveFile path format buf = do
  result <- try write :: IO (Either SomeException ())
  pure (either (Left . T.pack . show) Right result)
  where
    write = do
      -- Before the file is opened, and with that emptied.
      rope <- evaluate (B.bufRope buf)
      withBinaryFile path WriteMode $ \h -> do
        when (formatBom format) (BS.hPut h utf8Bom)
        case formatEol format of
          LF -> Rope.hPutUtf8 h rope
          CRLF -> forM_ (Rope.toChunks rope) (BS.hPut h . TE.encodeUtf8 . T.replace "\n" "\r\n")
