{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8S
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Text.Lazy.Encoding
import qualified Data.Yaml                  as Y
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           System.Console.CmdArgs
import System.Console.CmdArgs.Explicit(process,helpText, HelpFormat(..))
import           System.Environment
import           System.Exit
import           Text.Printf
import Lib


data CurlRunnings = CurlRunnings {
  file :: FilePath
                         } deriving (Show, Data, Typeable, Eq)

argParser :: CurlRunnings
argParser = CurlRunnings { file = def &= typFile &= help "File to run"
                         } &= summary "curl-runnings 0.0.0" &= program "curl-runnings" &= help "Use the --help flag to see the basics of curl-runnings."

decodeFile :: FilePath -> IO (Either String SmokeSuite)
decodeFile specPath =
  case last $ T.splitOn "." (T.pack specPath) of
    "json" -> eitherDecode' <$> B.readFile specPath :: IO (Either String SmokeSuite)
    "yaml" -> Y.decodeEither <$> B8S.readFile specPath :: IO (Either String SmokeSuite)
    _ -> return . Left $ printf "Invalid specPath: %s. Please supply a yaml or json file" (T.pack specPath)

exitWithHelp :: IO a
exitWithHelp = do
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
        Right s       -> void $ runSuite s
        Left messgage -> putStrLn $ "Couldn't read input as json: " ++ messgage
