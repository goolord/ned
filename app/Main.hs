module Main (main) where

import Data.Maybe (listToMaybe)
import Ned.App (runNed)
import Ned.Selftest (selftest)
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \case
    "--selftest" : dir : rest -> selftest dir (listToMaybe rest)
    args -> runNed (listToMaybe args)
