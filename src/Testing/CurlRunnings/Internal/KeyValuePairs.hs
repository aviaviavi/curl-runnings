{-# LANGUAGE OverloadedStrings #-}

-- | A module defining the KeyValuePairs type. This type may be used to
-- represent a structure in a specification that is a collection of
-- key-vaue pairs. For example query parameters and URLEncoded request
-- bodies.
--
-- The module provides FromJSON and ToJSON instances for KeyValuePairs.
-- Valid KeyValuePairs JSON is a single JSON object where all values
-- are either String, Scienfific or Bool.

module Testing.CurlRunnings.Internal.KeyValuePairs
  ( KeyValuePairs (..)
  , KeyValuePair (..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import           Data.HashMap.Strict  as H
import qualified Data.Text            as T
import           Data.Text.Encoding   as T

-- | A container for a list of key-value pairs
newtype KeyValuePairs = KeyValuePairs [KeyValuePair] deriving Show

-- | A representation of a single key-value pair
data KeyValuePair = KeyValuePair T.Text T.Text deriving Show

instance ToJSON KeyValuePairs where
  toJSON (KeyValuePairs qs) =
    object (fmap (\(KeyValuePair k v) -> k .= toJSON v) qs)

instance FromJSON KeyValuePairs where
  parseJSON = withObject "keyValuePairs" parseKeyValuePairs where
    parseKeyValuePairs o = KeyValuePairs <$> traverse parseKeyValuePair (H.toList o)
    parseKeyValuePair (t, v) = KeyValuePair t <$> parseSingleValueType v

parseSingleValueType :: Value -> Parser T.Text
parseSingleValueType (Bool b)   = parseToText b
parseSingleValueType (String t) = return t
parseSingleValueType (Number n) = parseToText n
parseSingleValueType invalid    = typeMismatch "KeyValuePairs" invalid

parseToText :: (ToJSON a) => a -> Parser T.Text
parseToText = return . T.decodeUtf8 . LBS.toStrict . encode
