{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module defining the Header and Headers types and a parser with a FromJSON
-- instance for the Headers type.
--
-- Headers are parsed from a semi-colon separated sequence of key:value pairs.
-- Some examples:
--
-- > "key: value"
-- > "key1: value1; key2: value2"
--
-- Keys can be any sequence of ASCII characters excluding ':' and must not be
-- all whitespace. For example:
--
-- > " : value"
--
-- is invalid.
--
-- Values can be any sequence of ASCII characters excluding ';' and may be all
-- whitespace. For example:
--
-- > "key : "
--
-- is valid.
module Testing.CurlRunnings.Internal.Headers
    ( Header (..)
    , Headers (..)
    , parseHeaders
    ) where

import           Data.Aeson
import           Data.Aeson.Types           hiding (Parser)
import           Data.Bifunctor             (Bifunctor (..))
import           Data.Char                  (isAscii, isSpace)
import           Data.Functor               (void)
import qualified Data.Text                  as T
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | A representation of a single header
data Header =
  Header T.Text
         T.Text
  deriving (Show, Generic, Eq)

instance ToJSON Header

instance ToJSON Headers

-- | Simple container for a list of headers, useful for a vehicle for defining a
-- fromJSON
newtype Headers =
  HeaderSet [Header]
  deriving (Show, Generic, Eq)

instance FromJSON Headers where
  parseJSON a@(String v) =
    case parseHeaders v of
      Right h -> return h
      Left e  -> typeMismatch ("Header failure: " ++ T.unpack e) a
  parseJSON invalid = typeMismatch "Header" invalid

-- | Given a header text, attempt to parse it into Headers.
parseHeaders :: T.Text -> Either T.Text Headers
parseHeaders hs = let trimmed = T.strip hs in
  first (T.pack . errorBundlePretty) (Text.Megaparsec.parse headersParser "" trimmed)

type Parser = Parsec Void T.Text

headerColon :: Parser T.Text
headerColon = L.symbol space ":"

headerSemiColon :: Parser T.Text
headerSemiColon = L.symbol space ";"

endOfHeader :: Parser ()
endOfHeader = try (void headerSemiColon) <|> eof

headerParser :: Parser Header
headerParser = do
  key <- do
    firstChar <- satisfy asciiExcludingColonAndSpace
    rest <- takeWhileP (Just "header key") asciiExcludingColon <* headerColon
    return $ T.singleton firstChar <> rest
  value <- takeWhileP (Just "header value") asciiExcludingSemiColon <* endOfHeader
  return $ Header (T.strip key) (T.strip value) where
    asciiExcludingColon = asciiExcludingChar ':'
    asciiExcludingSemiColon = asciiExcludingChar ';'
    asciiExcludingColonAndSpace t = asciiExcludingColon t && not (isSpace t)
    asciiExcludingChar c t =  isAscii t && t /= c

headersParser :: Parser Headers
headersParser = HeaderSet <$> some headerParser
