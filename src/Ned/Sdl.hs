-- | The two SDL calls nano-ui-sdl has none of.
--
-- This is the only module that reaches past the toolkit to the window
-- underneath, so the one @foreign import@ each of them needs is written down
-- here and nowhere else.
module Ned.Sdl
  ( setWindowTitle
  , setWindowSize
  ) where

import Control.Monad (void)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (Ptr, castPtr)
import NanoUI.Backend.Sdl (SdlEnv (..))

foreign import ccall unsafe "SDL_SetWindowTitle"
  sdlSetWindowTitle :: Ptr () -> CString -> IO CBool

foreign import ccall unsafe "SDL_SetWindowSize"
  sdlSetWindowSize :: Ptr () -> CInt -> CInt -> IO CBool

setWindowTitle :: SdlEnv -> Text -> IO ()
setWindowTitle env t =
  BS.useAsCString (TE.encodeUtf8 t) (void . sdlSetWindowTitle (castPtr (sdlWindow env)))

setWindowSize :: SdlEnv -> Int -> Int -> IO ()
setWindowSize env w h =
  void (sdlSetWindowSize (castPtr (sdlWindow env)) (fromIntegral w) (fromIntegral h))
