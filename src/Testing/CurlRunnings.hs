{-# LANGUAGE OverloadedStrings #-}

-- | curl-runnings is a framework for writing declaratively writing curl based tests for your API's.
-- Write your test specifications with yaml or json, and you're done!
--
module Testing.CurlRunnings
    (
      runCase
    , runSuite
    , decodeFile
    ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8S
import qualified Data.ByteString.Lazy       as B
import qualified Data.HashMap.Strict        as H
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Yaml                  as Y
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Testing.CurlRunnings.Types
import           Text.Printf

-- | decode a json or yaml file into a suite object
decodeFile :: FilePath -> IO (Either String CurlSuite)
decodeFile specPath =
  case last $ T.splitOn "." (T.pack specPath) of
    "json" ->
      eitherDecode' <$> B.readFile specPath :: IO (Either String CurlSuite)
    "yaml" ->
      Y.decodeEither <$> B8S.readFile specPath :: IO (Either String CurlSuite)
    "yml" ->
      Y.decodeEither <$> B8S.readFile specPath :: IO (Either String CurlSuite)
    _ -> return . Left $ printf "Invalid spec path %s" (T.pack specPath)

-- | Run a single test case, and returns the result. IO is needed here since this method is responsible
-- for actually curling the test case endpoint and parsing the result.
runCase :: CurlCase -> IO CaseResult
runCase curlCase = do
  initReq <- parseRequest $ url curlCase
  response <- httpBS . setRequestBodyJSON (requestData curlCase) $ initReq {method = B8S.pack . show $ requestMethod curlCase}
  returnVal <-
    (return . decode . B.fromStrict $ getResponseBody response) :: IO (Maybe Value)
  let returnCode = getResponseStatusCode response
      assertionErrors =
        map fromJust $
        filter
          isJust
          [checkBody curlCase returnVal, checkCode curlCase returnCode]
  return $
    case assertionErrors of
      []       -> CasePass curlCase
      failures -> CaseFail curlCase failures

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x  = Just $ last x

printR :: Show a => a -> IO a
printR x = print x >> return x

-- | Runs the test cases in order and stop when an error is hit. Returns all the results
runSuite :: CurlSuite -> IO [CaseResult]
runSuite (CurlSuite cases) =
  foldM
    (\prevResults curlCase ->
       case safeLast prevResults of
         Just (CaseFail _ _) -> return prevResults
         Just (CasePass _) -> do
           result <- runCase curlCase >>= printR
           return $ prevResults ++ [result]
         Nothing -> do
           result <- runCase curlCase >>= printR
           return [result])
    []
    cases

-- | Check if the retrieved value fail's the case's assertion
checkBody :: CurlCase -> Maybe Value -> Maybe AssertionFailure
-- | We are looking for an exact payload match, and we have a payload to check
checkBody curlCase@(CurlCase _ _ _ _ (Just matcher@(Exactly expectedValue)) _) (Just receivedBody)
  | expectedValue /= receivedBody =
    Just $ DataFailure curlCase matcher (Just receivedBody)
  | otherwise = Nothing
-- | We are checking a list of expected subvalues, and we have a payload to check
checkBody curlCase@(CurlCase _ _ _ _ (Just matcher@(Contains expectedSubvalues)) _) (Just receivedBody)
  | jsonContainsAll receivedBody expectedSubvalues = Nothing
  | otherwise = Just $ DataFailure curlCase matcher (Just receivedBody)
-- | We expected a body but didn't get one
checkBody curlCase@(CurlCase _ _ _ _ (Just anything) _) Nothing = Just $ DataFailure curlCase anything Nothing
-- | No assertions on the body
checkBody (CurlCase _ _ _ _ Nothing _) _ = Nothing

-- | Does the json value contain all of these sub-values?
jsonContainsAll :: Value -> [JsonSubExpr] -> Bool
jsonContainsAll jsonValue =
  all $ \match -> case match  of
          ValueMatch subval -> subval `elem` traverseValue jsonValue
          KeyValueMatch key subval ->
            containsKeyVal jsonValue key subval

-- | Does the json value contain the given key value pair?
containsKeyVal :: Value -> T.Text -> Value -> Bool
containsKeyVal jsonValue key val = case jsonValue of
  Object o -> isJust $ H.lookup key o
  _        -> False

-- | Fully traverse the json and return a list of all the values
traverseValue :: Value -> [Value]
traverseValue val =
  case val of
    Object o     -> val : concatMap traverseValue (H.elems o)
    Array o      -> val : concatMap traverseValue o
    n@(Number _) -> [n]
    s@(String _) -> [s]
    b@(Bool _)   -> [b]
    Null         -> []

-- | Verify the returned http status code is ok, construct the right failure
-- type if needed
checkCode :: CurlCase -> Int -> Maybe AssertionFailure
checkCode curlCase@(CurlCase _ _ _ _ _ (ExactCode expectedCode)) receivedCode
  | expectedCode /= receivedCode = Just $ StatusFailure curlCase receivedCode
  | otherwise = Nothing
checkCode curlCase@(CurlCase _ _ _ _ _ (AnyCodeIn l)) receivedCode
  | receivedCode `notElem` l = Just $ StatusFailure curlCase receivedCode
  | otherwise = Nothing
