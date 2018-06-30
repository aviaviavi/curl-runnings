{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.Monoid
import qualified Data.Text                     as T
import           Data.Version                  (showVersion)
import           Paths_curl_runnings           (version)
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
  verbosity &=
  help "Use the --file or -f flag to specify an intput file spec to run"

runFile :: FilePath -> Verbosity -> IO ()
runFile "" _ =
  putStrLn
    "Please specify an input file with the --file (-f) flag or use --help for more information"
runFile path verbosityLevel = do
  home <- getEnv "HOME"
  suite <- decodeFile . T.unpack $ T.replace "~" (T.pack home) (T.pack path)
  case suite of
    Right s -> do
      results <- runSuite s $ toLogLevel verbosityLevel
      if any isFailing results
        then putStrLn (T.unpack $ makeRed "Some tests failed") >>
             exitWith (ExitFailure 1)
        else putStrLn . T.unpack $ makeGreen "All tests passed!"
    Left message ->
      putStrLn . T.unpack . makeRed . T.pack $
      "Couldn't read input json or yaml file: " <> message

toLogLevel :: Verbosity -> LogLevel
toLogLevel v = toEnum $ fromEnum v

main :: IO ()
main = do
  userArgs <- cmdArgs argParser
  verbosityLevel <- getVerbosity
  runFile (file userArgs) verbosityLevel
