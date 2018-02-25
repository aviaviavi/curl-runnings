{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Lazy.Encoding
import GHC.Generics
import Lib
import Network.HTTP.Conduit
import Network.HTTP.Simple

main :: IO ()
main = do
  suite <- eitherDecode' <$> B.readFile "app/test.json" :: IO (Either String SmokeSuite)
  case suite of
    Right s -> void $ runSuite s
    Left messgage -> putStrLn $ "Couldn't read input as json: " ++ messgage
