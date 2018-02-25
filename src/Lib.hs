{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      SmokeSuite(..)
    , SmokeCase(..)
    , HttpMethod(..)
    , JsonMatcher(..)
    , StatusCodeMatcher(..)
    , AssertionFailure(..)
    , CaseResult(..)
    , runSuite
    ) where

import           Control.Monad
import           Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy    as B
import qualified Data.ByteString.Lazy.Char8    as B8
import qualified Data.ByteString.Char8    as B8S
import qualified Data.HashMap.Strict     as H
import           Data.Maybe
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Simple

data HttpMethod
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Show, Generic)

instance FromJSON HttpMethod

instance ToJSON HttpMethod

-- | A predicate to apply to the json body from the response
data JsonMatcher
  -- | Performs `==`
  = Exactly Value
  -- | Will check for membership, For objects, this object must be an exact subobject
  | Contains [Value]
  deriving (Show, Generic)

instance FromJSON JsonMatcher

instance ToJSON JsonMatcher

data StatusCodeMatcher
  = ExactCode Int
  | AnyCodeIn [Int]
  deriving (Show, Generic)

instance FromJSON StatusCodeMatcher

instance ToJSON StatusCodeMatcher

data SmokeCase = SmokeCase
  { name          :: String
  , url           :: String
  , requestMethod :: HttpMethod
  , requestData   :: Maybe Value
  , expectData    :: Maybe JsonMatcher
  , expectStatus  :: StatusCodeMatcher
  } deriving (Show, Generic)

instance FromJSON SmokeCase

instance ToJSON SmokeCase

data AssertionFailure
  = DataFailure SmokeCase
                (Maybe Value)
  | StatusFailure SmokeCase
                  Int
  | UnexpectedFailure

instance Show AssertionFailure where
  show (StatusFailure c receivedCode) = case expectStatus c of
    ExactCode code -> "Incorrect status code from " ++ url c ++ ". Expected: " ++ show code ++ ". Actual: " ++ show receivedCode
    AnyCodeIn codes -> "Incorrect status code from " ++ url c ++ ". Expected one of: " ++ show codes ++ ". Actual: " ++ show receivedCode
  show (DataFailure c val) = case expectData c of
    Just (Exactly v) -> "JSON response from " ++ url c ++ " didn't match spec. Expected: " ++ B8.unpack (encodePretty v) ++ ". Actual: " ++ B8.unpack (encodePretty val)
    Just (Contains v) -> "JSON response from " ++ url c ++ " didn't contain the matcher. Expected: " ++ B8.unpack (encodePretty v) ++ " to be each be subvalues in: " ++ B8.unpack (encodePretty val)
  show _ = "Unexpected Error D:"

data CaseResult
  = CasePass SmokeCase
  | CaseFail SmokeCase
             [AssertionFailure]

instance Show CaseResult where
  show (CasePass c) = "[PASS] " ++ name c
  show (CaseFail c failures) =
    "[FAIL] " ++ name c ++ "\n" ++ concatMap ((\s -> "\nAssertion failed: " ++ s) . (++"\n") . show) failures

newtype SmokeSuite =
  SmokeSuite [SmokeCase]
  deriving (Show, Generic)
instance FromJSON SmokeSuite
instance ToJSON SmokeSuite

-- data SmokeSuiteParser =
--   GenParser SmokeSuite
runCase :: SmokeCase -> IO CaseResult
runCase smokeCase = do
  initReq <- parseRequest $ url smokeCase
  response <- httpBS . setRequestBodyJSON (requestData smokeCase) $ initReq {method = B8S.pack . show $ requestMethod smokeCase}
  returnVal <-
    (return . decode . B.fromStrict $ getResponseBody response) :: IO (Maybe Value)
  let returnCode = getResponseStatusCode response
      assertionErrors =
        map fromJust $
        filter
          isJust
          [checkBody smokeCase returnVal, checkCode smokeCase returnCode]
  return $
    case assertionErrors of
      []       -> CasePass smokeCase
      failures -> CaseFail smokeCase failures

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x  = Just $ last x

printR :: Show a => a -> IO a
printR x = print x >> return x

-- | Runs the test cases in order and stop when an error is hit. Returns all the results
runSuite :: SmokeSuite -> IO [CaseResult]
runSuite (SmokeSuite cases) =
  foldM
    (\prevResults smokeCase ->
       case safeLast prevResults of
         Just (CaseFail _ _) -> return prevResults
         Just (CasePass _) -> do
           result <- runCase smokeCase >>= printR
           return $ prevResults ++ [result]
         Nothing -> do
           result <- runCase smokeCase >>= printR
           return [result])
    []
    cases

checkBody :: SmokeCase -> Maybe Value -> Maybe AssertionFailure
checkBody smokeCase@(SmokeCase _ _ _ _ (Just (Exactly expectedValue)) _) (Just receivedBody)
  | expectedValue /= receivedBody =
    Just $ DataFailure smokeCase (Just receivedBody)
  | otherwise = Nothing
checkBody smokeCase Nothing = Just $ DataFailure smokeCase Nothing
checkBody smokeCase@(SmokeCase _ _ _ _ (Just (Contains expectedSubvalues)) _) (Just receivedBody)
  | valueContainsAll receivedBody expectedSubvalues = Nothing
  | otherwise = Just $ DataFailure smokeCase (Just receivedBody)

valueContainsAll :: Value -> [Value] -> Bool
valueContainsAll val =
  all (\subval -> subval `elem` traverseValue val)

traverseValue :: Value -> [Value]
traverseValue val =
  case val of
    Object o     -> val : concatMap traverseValue (H.elems o)
    Array o      -> val : concatMap traverseValue o
    n@(Number _) -> [n]
    s@(String _) -> [s]
    b@(Bool _)   -> [b]
    Null         -> []

checkCode :: SmokeCase -> Int -> Maybe AssertionFailure
checkCode smokeCase@(SmokeCase _ _ _ _ _ (ExactCode expectedCode)) receivedCode
  | expectedCode /= receivedCode = Just $ StatusFailure smokeCase receivedCode
  | otherwise = Nothing
checkCode smokeCase@(SmokeCase _ _ _ _ _ (AnyCodeIn l)) receivedCode
  | receivedCode `notElem` l = Just $ StatusFailure smokeCase receivedCode
  | otherwise = Nothing
