{-# LANGUAGE CPP #-}

-- | Module for the purpose of Aeson compatibility
module Testing.CurlRunnings.Internal.Aeson
  ( module Map
  , KeyType
  , MapType
  , findWithDefault
  , fromText
  , toText
  ) where

#if __GLASGOW_HASKELL__ < 810
import Data.Text (Text)
#endif

#if MIN_VERSION_aeson(2,0,0)

import Data.Aeson.KeyMap as Map
import Data.Aeson.Key (Key, fromText, toText)

type MapType v = Map.KeyMap v
type KeyType = Key

#else

import Data.HashMap.Strict as Map

type MapType v = Map.HashMap Text v
type KeyType = Text

fromText :: Text -> Text
fromText = id

toText :: Text -> Text
toText = id

#endif

findWithDefault :: a -> KeyType -> MapType a -> a
findWithDefault def k kv = case Map.lookup k kv of
    Just v -> v
    _      -> def
{-# INLINABLE findWithDefault #-}
