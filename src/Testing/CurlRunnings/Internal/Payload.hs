{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module defining the Payload type. This is used in specifications to
-- represent request body data. Currently JSON and URLEncoded bodies are
-- supported.
--
-- The module provides a FromJSON instance to parse a Payload from a
-- specification.
--
-- Payload is parsed from an object containing two keys: `bodyType` and
-- `content`.
--
-- The value of the `bodyType` field must be either `json` or
-- `urlencoded` and this indicates how the request data should be encoded.
--
-- When `bodyType` is `urlencoded ` the value of the `content` field must be an
-- object with string, numeric or boolean values.
--
-- When `bodyType` is `json` the value of the `content` field will be used as
-- the JSON payload.
--
-- If `bodyType` is not present then the whole object is used as the JSON
-- payload.
--
-- Examples:
-- 1. A URLEncoded request payload:
--
-- >  requestData:
-- >     bodyType: urlencoded
-- >     content:
-- >       key1: value
-- >       key2: true
-- >       key3: 10.22
--
-- 2: A JSON request payload using `bodyType`:
--
-- >  requestData:
-- >     bodyType: json
-- >     content:
-- >       key1: value
-- >       key2: [1,2,3]
--
-- 3: A JSON request payload without using `bodyType`:
--
-- >  requestData:
-- >     key1: value
-- >     key2: [1,2,3]
--
module Testing.CurlRunnings.Internal.Payload
  ( Payload (..)
  ) where

import           Data.Aeson
import qualified Data.Char                                   as C
import qualified Data.Text                                   as T
import           GHC.Generics
import qualified Testing.CurlRunnings.Internal.Aeson         as A
import           Testing.CurlRunnings.Internal.KeyValuePairs

data Payload = JSON Value | URLEncoded KeyValuePairs deriving Generic

instance Show Payload where
  show (JSON v) = show v
  show (URLEncoded (KeyValuePairs xs)) = T.unpack $ T.intercalate "&" $ fmap (\(KeyValuePair k v) -> A.toText k <> "=" <> v) xs

payloadTagFieldName :: A.KeyType
payloadTagFieldName = "bodyType"

payloadContentsFieldName :: A.KeyType
payloadContentsFieldName = "content"

instance FromJSON Payload where
  parseJSON v = withObject "payload" parsePayload v where
    parsePayload o = if not (A.member payloadTagFieldName o) then return (JSON v) else genericParseJSON payloadOptions v
    payloadOptions = defaultOptions { sumEncoding = TaggedObject { tagFieldName = T.unpack . A.toText $ payloadTagFieldName
                                                                 , contentsFieldName = T.unpack . A.toText $ payloadContentsFieldName
                                                                 }
                                    , constructorTagModifier = fmap C.toLower
                                    }

instance ToJSON Payload where
  toJSON (JSON v) = object [(payloadTagFieldName, "json"), (payloadContentsFieldName, toJSON v)]
  toJSON (URLEncoded xs) = object [(payloadTagFieldName, "urlencoded"), (payloadContentsFieldName, toJSON xs)]
