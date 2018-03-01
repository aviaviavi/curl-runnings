{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data types for curl-runnings tests
module Testing.CurlRunnings.Types
  ( CurlSuite(..)
  , CurlCase(..)
  , HttpMethod(..)
  , JsonMatcher(..)
  , JsonSubExpr(..)
  , StatusCodeMatcher(..)
  , AssertionFailure(..)
  , CaseResult(..)
  , isPassing
  , isFailing
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text as T
import GHC.Generics
import Text.Printf
import Testing.CurlRunnings.Internal

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

instance FromJSON JsonMatcher

instance ToJSON JsonMatcher

-- | A matcher for a subvalue of a json payload
data JsonSubExpr
  -- | Assert some value anywhere in the json has a value equal to a given
  --  value. The motivation for this field is largely for checking contents of a
  --  top level array. It's also useful if you don't know the key ahead of time.
  = ValueMatch Value
  -- | Assert the key value pair can be found somewhere the json.
  | KeyValueMatch { matchKey :: T.Text
                  , matchValue :: Value }
  deriving (Show, Generic)

instance FromJSON JsonSubExpr

instance ToJSON JsonSubExpr

-- | Check the status code of a response. You can specify one or many valid codes.
data StatusCodeMatcher
  = ExactCode Int
  | AnyCodeIn [Int]
  deriving (Show, Generic)

instance FromJSON StatusCodeMatcher

instance ToJSON StatusCodeMatcher

-- | A single curl test case, the basic foundation of a curl-runnings test.
data CurlCase = CurlCase
  { name :: String -- ^ The name of the test case
  , url :: String -- ^ The target url to test
  , requestMethod :: HttpMethod -- ^ Berb to use for the request
  , requestData :: Maybe Value -- ^ Payload to send with the request, if any
  , expectData :: Maybe JsonMatcher -- ^ The assertions to make on the response payload, if any
  , expectStatus :: StatusCodeMatcher -- ^ Assertion about the status code returned by the target
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
  show UnexpectedFailure = "Unexpected Error D:"

-- | A type representing the result of a single curl, and all associated
-- assertions
data CaseResult
  = CasePass CurlCase
  | CaseFail CurlCase
             [AssertionFailure]

instance Show CaseResult where
  show (CasePass c) = makeGreen "[PASS] " ++ name c
  show (CaseFail c failures) =
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
isPassing (CasePass _) = True
isPassing (CaseFail _ _) = False

-- | Simple predicate that checks if the result is failing
isFailing :: CaseResult -> Bool
isFailing (CasePass _) = False
isFailing (CaseFail _ _) = True
