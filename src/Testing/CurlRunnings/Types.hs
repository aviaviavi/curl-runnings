{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}

-- | Data types for curl-runnings tests

module Testing.CurlRunnings.Types
  ( AssertionFailure(..)
  , Authentication(..)
  , CaseResult(..)
  , CurlCase(..)
  , CurlRunningsState(..)
  , CurlSuite(..)
  , FullQueryText
  , Header(..)
  , HeaderMatcher(..)
  , Headers(..)
  , HttpMethod(..)
  , Index(..)
  , InterpolatedQuery(..)
  , JsonMatcher(..)
  , JsonSubExpr(..)
  , KeyValuePair(..)
  , KeyValuePairs(..)
  , PartialHeaderMatcher(..)
  , Payload(..)
  , Query(..)
  , QueryError(..)
  , SingleQueryText
  , StatusCodeMatcher(..)
  , TLSCheckType(..)

  , isFailing
  , isPassing
  , logger
  , unsafeLogger

  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import qualified Data.Text                                   as T
import qualified Data.Vector                                 as V
import           GHC.Generics
import           Testing.CurlRunnings.Internal
import qualified Testing.CurlRunnings.Internal.Aeson         as A
import           Testing.CurlRunnings.Internal.Headers
import           Testing.CurlRunnings.Internal.KeyValuePairs
import           Testing.CurlRunnings.Internal.Payload
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
  -- | A list of matchers to make assertions that contains values exist in the response
  | Contains [JsonSubExpr]
  -- | A list of matchers to make assertions that contains values do not exist in the response
  | NotContains [JsonSubExpr]
  -- | We're specifiying both Contains and NotContains matchers
  | MixedContains [JsonMatcher]
  deriving (Show, Generic)

instance ToJSON JsonMatcher

instance FromJSON JsonMatcher where
  parseJSON (Object v)
    | justAndNotEmpty "exactly" v = Exactly <$> v .: "exactly"
    | justAndNotEmpty "contains" v && justAndNotEmpty "notContains" v = do
      c <- Contains <$> v .: "contains"
      n <- NotContains <$> v .: "notContains"
      return $ MixedContains [c, n]
    | justAndNotEmpty "contains" v = Contains <$> v .: "contains"
    | justAndNotEmpty "notContains" v = NotContains <$> v .: "notContains"
  parseJSON invalid = typeMismatch "JsonMatcher" invalid

justAndNotEmpty :: A.KeyType -> A.MapType Value -> Bool
justAndNotEmpty key obj =
  isJust (A.lookup key obj) && A.lookup key obj /= Just Null

-- | Simple predicate to check value constructor type
isContains :: JsonMatcher -> Bool
isContains (Contains _) = True
isContains _            = False

-- | Simple predicate to check value constructor type
isNotContains :: JsonMatcher -> Bool
isNotContains (NotContains _) = True
isNotContains _               = False

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
  = QueryParseError T.Text
                    T.Text
  -- | The retrieved a value of the wrong type or was otherwise operating on the
  -- wrong type of thing
  | QueryTypeMismatch T.Text
                      Value
  -- | The query was parse-able
  | QueryValidationError T.Text
  -- | Tried to access a value in a null object.
  | NullPointer T.Text -- full query
                T.Text -- message
                deriving (Generic)

instance Show QueryError where
  show (QueryParseError t q) = printf "error parsing query %s: %s" q $ T.unpack t
  show (NullPointer full part) = printf "null pointer in %s at %s" (T.unpack full) $ T.unpack part
  show (QueryTypeMismatch message val) = printf "type error: %s %s" message $ show val
  show (QueryValidationError message) = printf "invalid query: %s" message

instance ToJSON QueryError

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
  -- | Assert a key exists anywhere in the json
  | KeyMatch T.Text
  -- | Assert the key value pair can be found somewhere the json.
  | KeyValueMatch { matchKey   :: T.Text
                  , matchValue :: Value }
  deriving (Show, Generic)

instance FromJSON JsonSubExpr where
  parseJSON (Object v)
    | justAndNotEmpty "keyValueMatch" v =
      let toParse = fromJust $ A.lookup "keyValueMatch" v
      in case toParse of
           Object o -> KeyValueMatch <$> o .: "key" <*> o .: "value"
           _        -> typeMismatch "JsonSubExpr" toParse
    | justAndNotEmpty "keyMatch" v =
      let toParse = fromJust $ A.lookup "keyMatch" v
      in case toParse of
           String s -> return $ KeyMatch s
           _        -> typeMismatch "JsonSubExpr" toParse
    | justAndNotEmpty "valueMatch" v = ValueMatch <$> v .: "valueMatch"
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

data Authentication =
  BasicAuthentication T.Text T.Text
  deriving (Show, Generic)

instance FromJSON Authentication where
  parseJSON (Object o) = BasicAuthentication <$> (o .: "basic" >>= (.: "username")) <*> (o .: "basic" >>= (.: "password"))
  parseJSON invalid    = typeMismatch "Authentication" invalid
instance ToJSON Authentication

-- | A single curl test case, the basic foundation of a curl-runnings test.
data CurlCase = CurlCase
  { name             :: T.Text -- ^ The name of the test case
  , url              :: T.Text -- ^ The target url to test
  , requestMethod    :: HttpMethod -- ^ Verb to use for the request
  , requestData      :: Maybe Payload -- ^ Payload to send with the request, if any
  , queryParameters  :: Maybe KeyValuePairs -- ^ Query parameters to set in the request, if any
  , headers          :: Maybe Headers -- ^ Headers to send with the request, if any
  , auth             :: Maybe Authentication -- ^ Authentication to add to the request, if any
  , expectData       :: Maybe JsonMatcher -- ^ The assertions to make on the response payload, if any
  , expectStatus     :: StatusCodeMatcher -- ^ Assertion about the status code returned by the target
  , expectHeaders    :: Maybe HeaderMatcher -- ^ Assertions to make about the response headers, if any
  , allowedRedirects :: Maybe Int -- ^ Number of redirects to follow. Defaults to 10
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
  | UnexpectedFailure deriving (Generic)

instance ToJSON AssertionFailure

colorizeExpects :: String -> String
colorizeExpects t =
  let expectedColor = makeRed "Expected:"
      actualColor = makeRed "Actual:"
      replacedExpected = T.replace "Expected:" expectedColor (T.pack t)
  in T.unpack $ T.replace "Actual:" actualColor replacedExpected

instance Show AssertionFailure where
  show (StatusFailure c receivedCode) =
    case expectStatus c of
      ExactCode code ->
        colorizeExpects $
        printf
          "[%s] Incorrect status code from %s. Expected: %s. Actual: %s"
          (name c)
          (url c)
          (show code)
          (show receivedCode)
      AnyCodeIn codes ->
        colorizeExpects $
        printf
          "[%s] Incorrect status code from %s. Expected: %s. Actual: %s"
          (name c)
          (url c)
          (show codes)
          (show receivedCode)
  show (DataFailure curlCase expected receivedVal) =
    case expected of
      Exactly expectedVal ->
        colorizeExpects $
        printf
          "[%s] JSON response from %s didn't match spec. Expected: %s. Actual: %s"
          (name curlCase)
          (url curlCase)
          (T.unpack (pShow expectedVal))
          (T.unpack (pShow receivedVal))
      (Contains expectedVals) ->
        colorizeExpects $
        printf
          "[%s] JSON response from %s didn't contain the matcher. Expected: %s to each be subvalues in: %s"
          (name curlCase)
          (url curlCase)
          (T.unpack (pShow expectedVals))
          (T.unpack (pShow receivedVal))
      (NotContains expectedVals) ->
        colorizeExpects $
        printf
          "[%s] JSON response from %s did contain the matcher. Expected: %s not to be subvalues in: %s"
          (name curlCase)
          (url curlCase)
          (T.unpack (pShow expectedVals))
          (T.unpack (pShow receivedVal))
      (MixedContains expectedVals) ->
        colorizeExpects $
        printf
          "[%s] JSON response from %s didn't satisfy the matcher. Expected: %s to each be subvalues and %s not to be subvalues in: %s"
          (name curlCase)
          (url curlCase)
          (T.unpack (pShow (filter isContains expectedVals)))
          (T.unpack (pShow (filter isNotContains expectedVals)))
          (T.unpack (pShow receivedVal))
  show (HeaderFailure curlCase expected receivedHeaders) =
    colorizeExpects $
    printf
      "[%s] Headers from %s didn't contain expected headers. Expected: %s. Actual: %s"
      (name curlCase)
      (url curlCase)
      (show expected)
      (show receivedHeaders)
  show (QueryFailure curlCase queryErr) =
    colorizeExpects $
    printf "JSON query error in spec %s: %s" (name curlCase) (show queryErr)
  show UnexpectedFailure = "Unexpected Error D:"

-- | A type representing the result of a single curl, and all associated
-- assertions
data CaseResult
  = CasePass
      { curlCase            :: CurlCase
      , caseResponseHeaders :: Maybe Headers
      , caseResponseValue   :: Maybe Value
      , elapsedTime         :: Integer -- ^ Elapsed time
      }
  | CaseFail
      { curlCase            :: CurlCase
      , caseResponseHeaders :: Maybe Headers
      , caseResponseValue   :: Maybe Value
      , failures            :: [AssertionFailure]
      , elapsedTime         :: Integer -- ^ Elapsed time
      } deriving (Generic)

instance Show CaseResult where
  show CasePass{curlCase, elapsedTime} = T.unpack . makeGreen $ "[PASS] " <> (T.pack $ printf "%s (%0.2f seconds)" (name curlCase) (millisToS elapsedTime))
  show CaseFail{curlCase, failures, elapsedTime} =
    T.unpack $ makeRed "[FAIL] " <>
    name curlCase <>
    (T.pack $ printf " (%0.2f seconds) " (millisToS elapsedTime)) <>
    "\n" <>
    mconcat (map ((\s -> "\nAssertion failed: " <> s) . (<> "\n") . (T.pack . show)) failures)

instance ToJSON CaseResult where
  toJSON CasePass {curlCase, caseResponseHeaders, caseResponseValue, elapsedTime} =
    object
      [ "testPassed" .= (Bool True)
      , "case" .= curlCase
      , "responseHeaders" .= caseResponseHeaders
      , "responseValue" .= caseResponseValue
      , "elapsedTimeSeconds" .= millisToS elapsedTime
      ]
  toJSON CaseFail {curlCase, caseResponseHeaders, caseResponseValue, elapsedTime, failures} =
    object
      [ "testPassed" .= (Bool False)
      , "case" .= curlCase
      , "responseHeaders" .= caseResponseHeaders
      , "responseValue" .= caseResponseValue
      , "elapsedTimeSeconds" .= millisToS elapsedTime
      , "failures" .= failures
      ]

-- | A wrapper type around a set of test cases. This is the top level spec type
-- that we parse a test spec file into
data CurlSuite = CurlSuite
  { suiteCases      :: [CurlCase]
  , suiteCaseFilter :: Maybe T.Text
  } deriving (Show, Generic)

noFilterSuite :: [CurlCase] -> CurlSuite
noFilterSuite = flip CurlSuite Nothing

instance ToJSON CurlSuite

instance FromJSON CurlSuite where
  parseJSON (Object v)  = noFilterSuite <$> v .: "cases"
  parseJSON a@(Array _) = noFilterSuite <$> parseJSON a
  parseJSON invalid     = typeMismatch "JsonMatcher" invalid

-- | Simple predicate that checks if the result is passing
isPassing :: CaseResult -> Bool
isPassing CasePass {} = True
isPassing CaseFail {} = False

-- | Simple predicate that checks if the result is failing
isFailing :: CaseResult -> Bool
isFailing = not . isPassing

-- | A map of the system environment
type Environment = A.MapType T.Text

data TLSCheckType = SkipTLSCheck | DoTLSCheck deriving (Show, Eq)

-- | The state of a suite. Tracks environment variables, and all the test results so far
data CurlRunningsState = CurlRunningsState Environment [CaseResult] LogLevel TLSCheckType

logger :: CurlRunningsState -> CurlRunningsLogger
logger (CurlRunningsState _ _ l _) = makeLogger l

unsafeLogger :: Show a => CurlRunningsState -> CurlRunningsUnsafeLogger a
unsafeLogger (CurlRunningsState _ _ l _) = makeUnsafeLogger l

-- | A single lookup operation in a json query
data Index
  -- | Drill into the json of a specific test case. The RESPONSES object is
  -- accessible as an array of values that have come back from previous test
  -- cases
  = CaseResultIndex Integer
  -- | A standard json key lookup.
  | KeyIndex T.Text
  -- | A standard json array index lookup.
  | ArrayIndex Integer
  deriving (Show)

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

-- | The full string in which a query appears, eg "prefix-${{RESPONSES[0].key.another_key[0].last_key}}"
type FullQueryText = T.Text
-- | The string for one query given the FullQueryText above, the single query text would be RESPONSES[0].key.another_key[0].last_key
type SingleQueryText = T.Text
