module Main (main) where

import Data.Maybe (listToMaybe)
import qualified Data.Text.IO as T
import Ned.App (runNed)
import Ned.Config (defaultConfigText)
import Ned.Selftest (selftest)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    "--selftest" : dir : rest -> selftest dir (listToMaybe rest)
    ["--default-config"] -> T.putStr defaultConfigText
    paths -> runNed paths
