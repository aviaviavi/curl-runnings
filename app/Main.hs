{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8           as B8S
import qualified Data.ByteString.Lazy            as B
import qualified Data.Text                       as T
import           Data.Version                    (showVersion)
import qualified Data.Yaml                       as Y
import           Paths_curl_runnings             (version)
import           System.Console.CmdArgs
import           System.Environment
import           System.Exit
import           Testing.CurlRunnings
import           Testing.CurlRunnings.Internal
import           Testing.CurlRunnings.Types
import           Text.Printf

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

-- | decode a json or yaml file into a suite object
decodeFile :: FilePath -> IO (Either String CurlSuite)
decodeFile specPath =
  case last $ T.splitOn "." (T.pack specPath) of
    "json" ->
      eitherDecode' <$> B.readFile specPath :: IO (Either String CurlSuite)
    "yaml" ->
      Y.decodeEither <$> B8S.readFile specPath :: IO (Either String CurlSuite)
    "yml" ->
      Y.decodeEither <$> B8S.readFile specPath :: IO (Either String CurlSuite)
    _ ->
      return . Left $
      printf
        "Invalid specPath: %s. Please supply a yaml or json file"
        (T.pack specPath)

main :: IO ()
main = do
  mainArgs <- cmdArgs argParser
  case file mainArgs of
    "" ->
      putStrLn
        "Please specify an input file with the -f flag or use --help for more information"
    path -> do
      home <- getEnv "HOME"
      suite <- decodeFile . T.unpack $ T.replace "~" (T.pack home) (T.pack path)
      case suite of
        Right s -> do
          results <- runSuite s
          if any isFailing results
            then putStrLn (makeRed "Some tests failed") >>
                 exitWith (ExitFailure 1)
            else putStrLn $ makeGreen "All tests passed!"
        Left messgage -> putStrLn $ "Couldn't read input as json: " ++ messgage
