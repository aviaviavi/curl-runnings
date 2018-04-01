{-# LANGUAGE OverloadedStrings   #-}

-- | Internal module for parsing string based directives inside of curl runnings
-- suites. Use this module at your own risk, it may change. Currently, string
-- interpolation can be performed, where interpolated values are json quries
-- into responses from past test cases.
--
-- > "$<SUITE[0].key[0].another_key>"
--
-- here the `SUITE` keyword references the results of previous test cases. Here, the
-- whole string is a query, so if the value referenced by this query is itself a
-- json value, the entire value will replace this string in a json matcher.
-- Additionally, interpolation of the form:
--
-- >
-- > "some text to interpolate with $<SUITE[0].key.key>"
-- >
--
-- will substitute a string found at the specified query
-- and subsitute the string.
--
-- Rules for the language are similar to JQ or regular JSON indexing rules. All
-- queries must start with a SUITE[integer] index, and be written between a
--
-- >
-- >  $< ... >
-- >
--
-- to signify an interpolation. You can have mutliple queries inside a
-- single string, but if interpolation is occuring, then the query specified
-- must resolve to a string value. Otheriwse, a type error will be thrown.
module Testing.CurlRunnings.Internal.Parser
    (
      parseQuery
    ) where

import           Data.List
import qualified Data.Text                  as T
import           Data.Void
import           Testing.CurlRunnings.Types
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Given some query text, attempt to parse it to a list of interplated query objects. This data representation may change.
parseQuery :: FullQueryText -> Either QueryError [InterpolatedQuery]
parseQuery q =
  let trimmed = T.strip q
  in case Text.Megaparsec.parse parseFullTextWithQuery "" trimmed of
       Right a -> Right a
       Left a  -> Left $ QueryParseError (T.pack $ parseErrorPretty a) q

type Parser = Parsec Void T.Text

parseSuiteIndex' :: Parser Index
parseSuiteIndex' = do
  notFollowedBy gtlt
  _ <- string "SUITE"
  (ArrayIndex i) <- arrayIndexParser
  return $ CaseResultIndex i

spaceOrDot :: Parser ()
spaceOrDot = (try $ char '.' >> space) <|> space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceOrDot

symbol :: T.Text -> Parser T.Text
symbol = L.symbol spaceOrDot

inGTLT :: Parser a -> Parser a
inGTLT = between (symbol "$<") (string ">")

gtlt :: Parser T.Text
gtlt = symbol "<" <|> symbol ">"

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

bracket :: Parser T.Text
bracket = symbol "[" <|> symbol "]"

integer :: Parser Integer
integer = lexeme L.decimal

dot :: Parser T.Text
dot = symbol "."

arrayIndexParser :: Parser Index
arrayIndexParser = notFollowedBy gtlt >> ArrayIndex <$> brackets integer

endingChars :: Parser T.Text
endingChars = dot <|> eol <|> bracket <|> gtlt

keyIndexParser :: Parser Index
keyIndexParser = do
  notFollowedBy endingChars
  (lexeme . try) ((KeyIndex . T.pack) <$> p)
  where
    p = (:) <$> letterChar <*> many (noneOf ['.', '[', ']', '<', '>', ' '])

jsonIndexParser :: Parser [Index]
jsonIndexParser = do
  _ <- leadingText
  inGTLT $ some (parseSuiteIndex' <|> keyIndexParser <|> arrayIndexParser)

interpolatedQueryParser :: Parser InterpolatedQuery
interpolatedQueryParser = do
  text <- leadingText
  q <- jsonIndexParser
  return $ InterpolatedQuery (T.pack text) (Query q)

leadingText :: Parser String
leadingText = manyTill anyChar $ lookAhead $ symbol "$<"

noQueryText :: Parser InterpolatedQuery
noQueryText = do
  str <- some anyChar
  eof
  if "$<" `isInfixOf` str
    then fail "invalid `$<` found"
    else return $ LiteralText $ T.pack str

parseFullTextWithQuery :: Parser [InterpolatedQuery]
parseFullTextWithQuery = many ((try interpolatedQueryParser) <|> noQueryText)

