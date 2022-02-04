{-# LANGUAGE OverloadedStrings #-}

-- | Internal module for parsing string based directives inside of curl runnings
-- suites. Use this module at your own risk, it may change. Currently, string
-- interpolation can be performed, where interpolated values are json quries
-- into responses from past test cases.
--
-- > "$<RESPONSES[0].key[0].another_key>"
--
-- here the `RESPONSES` keyword references the results of previous test cases. Here, the
-- whole string is a query, so if the value referenced by this query is itself a
-- json value, the entire value will replace this string in a json matcher.
-- Additionally, interpolation of the form:
--
-- >
-- > "some text to interpolate with $<RESPONSES[0].key.key>"
-- >
--
-- will substitute a string found at the specified query
-- and subsitute the string.
--
-- Rules for the language are similar to JQ or regular JSON indexing rules. All
-- queries must start with a RESPONSES[integer] index, and be written between a
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

import           Data.Bifunctor             ()
import           Data.Char                  ()
import           Data.List                  (isInfixOf)
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
       Right a -> Right a >>= validateQuery
       Left a  -> Left $ QueryParseError (T.pack $ errorBundlePretty a) q

-- | Once we have parsed a query successfully, ensure that it is a legal query
validateQuery :: [InterpolatedQuery] -> Either QueryError [InterpolatedQuery]
-- If we have a json indexing query, it needs to start by indexing the special
-- RESPONSES array
validateQuery q@(InterpolatedQuery _ (Query (CaseResultIndex _:_)):_) = Right q
validateQuery q@(NonInterpolatedQuery  (Query (CaseResultIndex _:_)):_) = Right q
-- If the RESPONSES array is not indexed, it's not valid, as we don't know which
-- response to look at
validateQuery (InterpolatedQuery _ (Query  _):_) = Left $ QueryValidationError "JSON interpolation must begin by indexing into RESPONSES"
validateQuery (NonInterpolatedQuery (Query _):_) = Left $ QueryValidationError "JSON interpolation must begin by indexing into RESPONSES"
-- Otherwise, we're good!
validateQuery q = Right q

type Parser = Parsec Void T.Text

parseSuiteIndex' :: Parser Index
parseSuiteIndex' = do
  notFollowedBy gtlt
  _ <- string "RESPONSES" <|> string "SUITE"
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

braces ::  Parser a -> Parser a
braces = between (symbol "${") (string "}")

brace :: Parser T.Text
brace = symbol "{" <|> symbol "}"

integer :: Parser Integer
integer = lexeme $ L.signed spaceOrDot L.decimal

dot :: Parser T.Text
dot = symbol "."

arrayIndexParser :: Parser Index
arrayIndexParser = notFollowedBy gtlt >> ArrayIndex <$> brackets integer

environmentVariableParser :: Parser Query
environmentVariableParser = do
  notFollowedBy endingChars
  (EnvironmentVariable . T.pack) <$> braces (lexeme $ many (noneOf ['[', ']', '<', '>', ' ', '{', '}']))

endingChars :: Parser T.Text
endingChars = dot <|> eol <|> bracket <|> gtlt <|> brace

keyIndexParser :: Parser Index
keyIndexParser = do
  notFollowedBy endingChars
  (lexeme . try) ((KeyIndex . T.pack) <$> p)
  where
    p = (:) <$> (letterChar <|> (char '_')) <*> many (noneOf ['.', '[', ']', '<', '>', ' ', '{', '}'])

jsonIndexParser :: Parser Query
jsonIndexParser =
  leadingText >>
  (inGTLT $ some (parseSuiteIndex' <|> keyIndexParser <|> arrayIndexParser)) >>=
  return . Query

interpolatedQueryParser :: Parser InterpolatedQuery
interpolatedQueryParser = do
  text <- leadingText
  q <- environmentVariableParser <|> jsonIndexParser
  if null text then return $ NonInterpolatedQuery q
  else return $ InterpolatedQuery (T.pack text) q

leadingText :: Parser String
leadingText = manyTill anySingle $ lookAhead (symbol "$<" <|> "${")

noQueryText :: Parser InterpolatedQuery
noQueryText = do
  str <- some anySingle
  eof
  if "$<" `isInfixOf` str
    then fail "invalid `$<` found"
    else if "${" `isInfixOf` str
      then fail "invalid `${` found"
      else return $ LiteralText $ T.pack str

parseFullTextWithQuery :: Parser [InterpolatedQuery]
parseFullTextWithQuery = many ((try interpolatedQueryParser) <|> noQueryText)
