module Main (main) where

import qualified Data.Text.IO as T
import Ned.App (runNed)
import Ned.Config (defaultConfigText)
import Ned.Selftest (selftest)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStr, stderr)

-- | What a run of the program is for. A later flag overrides an earlier one.
data Mode
  = Edit
  | Selftest FilePath
  | DefaultConfig
  | Help

options :: [OptDescr (Mode -> Mode)]
options =
  [ Option [] ["selftest"] (ReqArg (const . Selftest) "DIR") "Run the self-test on FILE, writing screenshots and a log to DIR"
  , Option [] ["default-config"] (NoArg (const DefaultConfig)) "Print every setting with its default"
  , Option ['h'] ["help"] (NoArg (const Help)) "Show this help"
  ]

usage :: IO String
usage = do
  name <- getProgName
  let header =
        unlines
          [ "Usage: " <> name <> " [FILE...]"
          , "       " <> name <> " --selftest DIR [FILE]"
          , "       " <> name <> " --default-config"
          ]
  pure (usageInfo header options)

failWith :: [String] -> IO a
failWith errs = do
  help <- usage
  hPutStr stderr (concat errs <> help)
  exitFailure

main :: IO ()
main = do
  (flags, paths, errs) <- getOpt Permute options <$> getArgs
  if not (null errs)
    then failWith errs
    else case (foldl (flip id) Edit flags, paths) of
      (Help, _) -> usage >>= putStr
      (Edit, _) -> runNed paths
      (Selftest dir, []) -> selftest dir Nothing
      (Selftest dir, [file]) -> selftest dir (Just file)
      (Selftest _, _) -> failWith ["--selftest takes at most one FILE\n"]
      (DefaultConfig, []) -> T.putStr defaultConfigText
      (DefaultConfig, _) -> failWith ["--default-config takes no FILE\n"]
