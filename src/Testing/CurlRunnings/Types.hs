{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data types for curl-runnings tests

module Testing.CurlRunnings.Types
  ( AssertionFailure(..)
  , CaseResult(..)
  , CurlSuite(..)
  , CurlCase(..)
  , Header(..)
  , HeaderMatcher(..)
  , Headers(..)
  , HttpMethod(..)
  , JsonMatcher(..)
  , JsonSubExpr(..)
  , PartialHeaderMatcher(..)
  , StatusCodeMatcher(..)
  , QueryError(..)
  , Index(..)
  , Query(..)
  , InterpolatedQuery(..)
  , FullQueryText
  , SingleQueryText
  , CurlRunningsState(..)

  , isFailing
  , isPassing
  , logger
  , unsafeLogger

  ) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8    as B8
import           Data.Either
import qualified Data.HashMap.Strict           as H
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import           GHC.Generics
import           Testing.CurlRunnings.Internal
import           Text.Printf

-- | A basic enum for supported HTTP verbs
data HttpMethod
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  deriving (Show, Generic)

instance FromJSON HttpMethod

instance ToJSON HttpMethod

-- | A predicate to apply to the json body from the response
data JsonMatcher
  -- | Performs `==`
  = Exactly Value
  -- | A list of matchers to make assertions about some subset of the response.
  | Contains [JsonSubExpr]
  deriving (Show, Generic)

instance ToJSON JsonMatcher

instance FromJSON JsonMatcher where
  parseJSON (Object v)
    | isJust $ H.lookup "exactly" v = Exactly <$> v .: "exactly"
    | isJust $ H.lookup "contains" v = Contains <$> v .: "contains"
  parseJSON invalid = typeMismatch "JsonMatcher" invalid

-- | A representation of a single header
data Header =
  Header T.Text
         T.Text
  deriving (Show, Generic)

instance ToJSON Header

-- | Simple container for a list of headers, useful for a vehicle for defining a
-- fromJSON
data Headers =
  HeaderSet [Header]
  deriving (Show, Generic)

instance ToJSON Headers

-- | Specify a key, value, or both to match against in the returned headers of a
-- response.
data PartialHeaderMatcher =
  PartialHeaderMatcher (Maybe T.Text)
                       (Maybe T.Text)
  deriving (Show, Generic)
instance ToJSON PartialHeaderMatcher

-- | Collection of matchers to run against a single curl response
data HeaderMatcher =
  HeaderMatcher [PartialHeaderMatcher]
  deriving (Show, Generic)

instance ToJSON HeaderMatcher

-- | Different errors relating to querying json from previous test cases
data QueryError
  -- | The query was malformed and couldn't be parsed
  = QueryParseError T.Text T.Text
  -- | The retrieved a value of the wrong type or was otherwise operating on the
  -- wrong type of thing
  | QueryTypeMismatch T.Text
                      Value
  -- | The query was parse-able
  | QueryValidationError T.Text
  -- | Tried to access a value in a null object
  | NullPointer T.Text
                T.Text

instance Show QueryError where
  show (QueryParseError t q) = printf "error parsing query %s: %s" q $ T.unpack t
  show (NullPointer full part) = printf "null pointer in %s at %s" (T.unpack full) $ T.unpack part
  show (QueryTypeMismatch message val) = printf "type error: %s %s" (message) $ show val
  show (QueryValidationError message) = printf "invalid query: %s" message

parseHeader :: T.Text -> Either T.Text Header
parseHeader str =
  case map T.strip $ T.splitOn ":" str of
    [key, val] -> Right $ Header key val
    anythingElse -> Left . T.pack $ "bad header found: " ++ (show anythingElse)

parseHeaders :: T.Text -> Either T.Text Headers
parseHeaders str =
  let _headers = filter (/= "") $ T.splitOn ";" str
      parses = map parseHeader _headers
  in case find isLeft parses of
       Just (Left failure) -> Left failure
       _ ->
         Right . HeaderSet $
         map
           (fromRight $
            error
              "Internal error parsing headers, this is a bug in curl runnings :(")
           parses

instance FromJSON Headers where
  parseJSON a@(String v) =
    case parseHeaders v of
      Right h      -> return h
      Left failure -> typeMismatch ("Header failure: " ++ T.unpack failure) a
  parseJSON invalid = typeMismatch "Header" invalid

instance FromJSON HeaderMatcher where
  parseJSON o@(String v) =
    either
      (\s -> typeMismatch ("HeaderMatcher: " ++ T.unpack s) o)
      (\(HeaderSet parsed) ->
         return . HeaderMatcher $
         map
           (\(Header key val) -> PartialHeaderMatcher (Just key) (Just val))
           parsed)
      (parseHeaders v)
  parseJSON (Object v) = do
    partial <- PartialHeaderMatcher <$> v .:? "key" <*> v .:? "value"
    return $ HeaderMatcher [partial]
  parseJSON (Array v) = mconcat . V.toList $ V.map parseJSON v
  parseJSON invalid = typeMismatch "HeaderMatcher" invalid

-- | A matcher for a subvalue of a json payload
data JsonSubExpr
  -- | Assert some value anywhere in the json has a value equal to a given
  --  value. The motivation for this field is largely for checking contents of a
  --  top level array. It's also useful if you don't know the key ahead of time.
  = ValueMatch Value
  -- | Assert the key value pair can be found somewhere the json.
  | KeyValueMatch { matchKey   :: T.Text
                  , matchValue :: Value }
  deriving (Show, Generic)

instance FromJSON JsonSubExpr where
  parseJSON (Object v)
    | isJust $ H.lookup "keyValueMatch" v =
      let toParse = fromJust $ H.lookup "keyValueMatch" v
      in case toParse of
           Object o -> KeyValueMatch <$> o .: "key" <*> o .: "value"
           _        -> typeMismatch "JsonSubExpr" toParse
    | isJust $ H.lookup "valueMatch" v = ValueMatch <$> v .: "valueMatch"
  parseJSON invalid = typeMismatch "JsonSubExpr" invalid
instance ToJSON JsonSubExpr

-- | Check the status code of a response. You can specify one or many valid codes.
data StatusCodeMatcher
  = ExactCode Int
  | AnyCodeIn [Int]
  deriving (Show, Generic)

instance ToJSON StatusCodeMatcher

instance FromJSON StatusCodeMatcher where
  parseJSON obj@(Number _) = ExactCode <$> parseJSON obj
  parseJSON obj@(Array _)  = AnyCodeIn <$> parseJSON obj
  parseJSON invalid        = typeMismatch "StatusCodeMatcher" invalid

-- | A single curl test case, the basic foundation of a curl-runnings test.
data CurlCase = CurlCase
  { name          :: String -- ^ The name of the test case
  , url           :: String -- ^ The target url to test
  , requestMethod :: HttpMethod -- ^ Verb to use for the request
  , requestData   :: Maybe Value -- ^ Payload to send with the request, if any
  , headers       :: Maybe Headers -- ^ Headers to send with the request, if any
  , expectData    :: Maybe JsonMatcher -- ^ The assertions to make on the response payload, if any
  , expectStatus  :: StatusCodeMatcher -- ^ Assertion about the status code returned by the target
  , expectHeaders :: Maybe HeaderMatcher -- ^ Assertions to make about the response headers, if any
  } deriving (Show, Generic)

instance FromJSON CurlCase

instance ToJSON CurlCase

-- | Represents the different type of test failures we can have. A single test case
-- | might return many assertion failures.
data AssertionFailure
  -- | The json we got back was wrong. We include this redundant field (it's
  -- included in the CurlCase field above) in order to enforce at the type
  -- level that we have to be expecting some data in order to have this type of
  -- failure.
  = DataFailure CurlCase
                JsonMatcher
                (Maybe Value)
  -- | The status code we got back was wrong
  | StatusFailure CurlCase
                  Int
  -- | The headers we got back were wrong
  | HeaderFailure CurlCase
                  HeaderMatcher
                  Headers
  -- | Something went wrong with a test case json query
  | QueryFailure CurlCase
                 QueryError
  -- | Something else
  | UnexpectedFailure

instance Show AssertionFailure where
  show (StatusFailure c receivedCode) =
    case expectStatus c of
      ExactCode code ->
        printf
          "Incorrect status code from %s. Expected: %s. Actual: %s"
          (url c)
          (show code)
          (show receivedCode)
      AnyCodeIn codes ->
        printf
          "Incorrect status code from %s. Expected one of: %s. Actual: %s"
          (url c)
          (show codes)
          (show receivedCode)
  show (DataFailure curlCase expected receivedVal) =
    case expected of
      Exactly expectedVal ->

        printf
          "JSON response from %s didn't match spec. Expected: %s. Actual: %s"
          (url curlCase)
          (B8.unpack (encodePretty expectedVal))
          (B8.unpack (encodePretty receivedVal))
      (Contains expectedVals) ->
        printf
          "JSON response from %s didn't contain the matcher. Expected: %s to be each be subvalues in: %s"
          (url curlCase)
          (B8.unpack (encodePretty expectedVals))
          (B8.unpack (encodePretty receivedVal))
  show (HeaderFailure curlCase expected receivedHeaders) =
    printf
      "Headers from %s didn't contain expected headers. Expected headers: %s. Received headers: %s"
      (url curlCase)
      (show expected)
      (show receivedHeaders)
  show (QueryFailure curlCase queryErr) =
    printf
      "JSON query error in spec %s: %s"
      (name curlCase)
      (show queryErr)
  show UnexpectedFailure = "Unexpected Error D:"

-- | A type representing the result of a single curl, and all associated
-- assertions
data CaseResult
  = CasePass CurlCase
             (Maybe Headers)
             (Maybe Value)
  | CaseFail CurlCase
             (Maybe Headers)
             (Maybe Value)
             [AssertionFailure]

instance Show CaseResult where
  show (CasePass c _ _) = makeGreen "[PASS] " ++ name c
  show (CaseFail c _ _ failures) =
    makeRed "[FAIL] " ++
    name c ++
    "\n" ++
    concatMap ((\s -> "\nAssertion failed: " ++ s) . (++ "\n") . show) failures

-- | A wrapper type around a set of test cases. This is the top level spec type
-- that we parse a test spec file into
newtype CurlSuite =
  CurlSuite [CurlCase]
  deriving (Show, Generic)

instance FromJSON CurlSuite

instance ToJSON CurlSuite

-- | Simple predicate that checks if the result is passing
isPassing :: CaseResult -> Bool
isPassing CasePass {} = True
isPassing CaseFail {} = False

-- | Simple predicate that checks if the result is failing
isFailing :: CaseResult -> Bool
isFailing = not . isPassing

-- | A map of the system environment
type Environment = H.HashMap T.Text T.Text

-- | The state of a suite. Tracks environment variables, and all the test results so far
data CurlRunningsState = CurlRunningsState Environment [CaseResult] LogLevel

logger :: CurlRunningsState -> CurlRunningsLogger
logger (CurlRunningsState _ _ l) = makeLogger l

unsafeLogger :: Show a => CurlRunningsState -> CurlRunningsUnsafeLogger a
unsafeLogger (CurlRunningsState _ _ l) = makeUnsafeLogger l

-- | A single lookup operation in a json query
data Index
  -- | Drill into the json of a specific test case. The SUITE object is
  -- accessible as an array of values that have come back from previous test
  -- cases
  = CaseResultIndex Integer
  -- | A standard json key lookup.
  | KeyIndex T.Text
  -- | A standard json array index lookup.
  | ArrayIndex Integer
  deriving (Show)

printOriginalQuery (CaseResultIndex t) = "SUITE[" ++ show t ++ "]"
printOriginalQuery (KeyIndex key)      = "." ++ T.unpack key
printOriginalQuery (ArrayIndex i)      = printf "[%d]" i

-- | A single entity to be queries from a json value
data Query =
  -- | A single query contains a list of discrete index operations
  Query [Index] |
  -- | Lookup a string in the environment
  EnvironmentVariable T.Text
  deriving (Show)

-- | A distinct parsed unit in a query
data InterpolatedQuery
  -- | Regular text, no query
  = LiteralText T.Text
  -- | Some leading text, then a query
  | InterpolatedQuery T.Text
                      Query
  -- | Just a query, no leading text
  | NonInterpolatedQuery Query
  deriving (Show)

printQueryString :: InterpolatedQuery -> String
printQueryString (LiteralText t) = show t
printQueryString (InterpolatedQuery raw (Query indexes)) = printf "%s$<%s>" raw (concat $ map show indexes)
printQueryString (NonInterpolatedQuery (Query indexes)) = printf "$<%s>" (concat $ map show indexes)

-- | The full string in which a query appears, eg "prefix-${{SUITE[0].key.another_key[0].last_key}}"
type FullQueryText = T.Text
-- | The string for one query given the FullQueryText above, the single query text would be SUITE[0].key.another_key[0].last_key
type SingleQueryText = T.Text
