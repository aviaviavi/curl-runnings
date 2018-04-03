{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import qualified Data.Text                       as T
import           Data.Version                    (showVersion)
import           Paths_curl_runnings             (version)
import           System.Console.CmdArgs
import           System.Environment
import           System.Exit
import           Testing.CurlRunnings
import           Testing.CurlRunnings.Internal
import           Testing.CurlRunnings.Types

-- | Command line flags
data CurlRunnings = CurlRunnings
  { file :: FilePath
  } deriving (Show, Data, Typeable, Eq)

-- | cmdargs object
argParser :: CurlRunnings
argParser =
  CurlRunnings {file = def &= typFile &= help "File to run"} &=
  summary ("curl-runnings " ++ showVersion version) &=
  program "curl-runnings" &=
  help "Use the --file or -f flag to specify an intput file spec to run"

runFile :: FilePath -> IO ()
runFile "" =
  putStrLn
    "Please specify an input file with the --file (-f) flag or use --help for more information"
runFile path = do
      home <- getEnv "HOME"
      suite <- decodeFile . T.unpack $ T.replace "~" (T.pack home) (T.pack path)
      case suite of
        Right s -> do
          results <- runSuite s
          if any isFailing results
            then putStrLn (makeRed "Some tests failed") >>
                 exitWith (ExitFailure 1)
            else putStrLn $ makeGreen "All tests passed!"
        Left messgage -> putStrLn . makeRed $ "Couldn't read input json or yaml file: " ++ messgage

main :: IO ()
main = cmdArgs argParser >>= runFile . file
