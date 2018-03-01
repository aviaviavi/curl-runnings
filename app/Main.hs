{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Char8           as B8S
import qualified Data.ByteString.Lazy            as B
import qualified Data.Text                       as T
import qualified Data.Yaml                       as Y
import           System.Console.CmdArgs
import           System.Console.CmdArgs.Explicit (HelpFormat (..), helpText)
import           System.Environment
import           System.Exit
import           Testing.CurlRunnings
import           Testing.CurlRunnings.Types
import           Testing.CurlRunnings.Internal
import           Text.Printf


-- | Command line flags
data CurlRunnings = CurlRunnings {
  file :: FilePath
                         } deriving (Show, Data, Typeable, Eq)

-- | cmdargs object
argParser :: CurlRunnings
argParser = CurlRunnings { file = def &= typFile &= help "File to run"
                         } &= summary "curl-runnings 0.0.0" &= program "curl-runnings" &= help "Use the --help flag to see the basics of curl-runnings."

-- | decode a json or yaml file into a suite object
decodeFile :: FilePath -> IO (Either String SmokeSuite)
decodeFile specPath =
  case last $ T.splitOn "." (T.pack specPath) of
    "json" -> eitherDecode'  <$> B.readFile specPath   :: IO (Either String SmokeSuite)
    "yaml" -> Y.decodeEither <$> B8S.readFile specPath :: IO (Either String SmokeSuite)
    "yml"  -> Y.decodeEither <$> B8S.readFile specPath :: IO (Either String SmokeSuite)
    _ -> return . Left $ printf "Invalid specPath: %s. Please supply a yaml or json file" (T.pack specPath)

exitWithHelp :: IO a
exitWithHelp = do
  -- add the list of flags to add to help here. an odd quirk of cmdargs...
  putStr $ show $ helpText ["file"] HelpFormatDefault $ cmdArgsMode argParser
  exitSuccess

main :: IO ()
main = do
  mainArgs <- cmdArgs argParser
  case file mainArgs of
    "" -> exitWithHelp
    path -> do
      home <- getEnv "HOME"
      suite <- decodeFile . T.unpack $ T.replace "~" (T.pack home) (T.pack path)
      case suite of
        Right s -> do
          results <- runSuite s
          if any isFailing results then
            putStrLn (makeRed "Some tests failed") >> exitWith (ExitFailure 1)
          else
            putStrLn $ makeGreen "All tests passed!"
        Left messgage -> putStrLn $ "Couldn't read input as json: " ++ messgage
