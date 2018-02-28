-- | curl-runnings is a framework for writing declaratively writing curl based tests for your API's.
-- Write your test specifications with yaml or json, and you're done!
--
module Testing.CurlRunnings
    (
      runCase
    , runSuite
    ) where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8      as B8S
import qualified Data.ByteString.Lazy       as B
import qualified Data.HashMap.Strict        as H
import           Data.Maybe
import qualified Data.Text                  as T
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Testing.CurlRunnings.Types

-- | Run a single test case, and returns the result. IO is needed here since this method is responsible
-- for actually curling the test case endpoint and parsing the result.
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

-- | Check if the retrieved value fail's the case's assertion
checkBody :: SmokeCase -> Maybe Value -> Maybe AssertionFailure
-- | We are looking for an exact payload match, and we have a payload to check
checkBody smokeCase@(SmokeCase _ _ _ _ (Just matcher@(Exactly expectedValue)) _) (Just receivedBody)
  | expectedValue /= receivedBody =
    Just $ DataFailure smokeCase matcher (Just receivedBody)
  | otherwise = Nothing
-- | We are checking a list of expected subvalues, and we have a payload to check
checkBody smokeCase@(SmokeCase _ _ _ _ (Just matcher@(Contains expectedSubvalues)) _) (Just receivedBody)
  | jsonContainsAll receivedBody expectedSubvalues = Nothing
  | otherwise = Just $ DataFailure smokeCase matcher (Just receivedBody)
-- | We expected a body but didn't get one
checkBody smokeCase@(SmokeCase _ _ _ _ (Just anything) _) Nothing = Just $ DataFailure smokeCase anything Nothing
-- | No assertions on the body
checkBody (SmokeCase _ _ _ _ Nothing _) _ = Nothing

-- | Does the json value contain all of these sub-values?
jsonContainsAll :: Value -> [JsonSubExpr] -> Bool
jsonContainsAll jsonValue =
  all (\match -> case match  of
          ValueMatch subval -> subval `elem` traverseValue jsonValue
          KeyValueMatch key subval ->
            containsKeyVal jsonValue key subval
      )

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
checkCode :: SmokeCase -> Int -> Maybe AssertionFailure
checkCode smokeCase@(SmokeCase _ _ _ _ _ (ExactCode expectedCode)) receivedCode
  | expectedCode /= receivedCode = Just $ StatusFailure smokeCase receivedCode
  | otherwise = Nothing
checkCode smokeCase@(SmokeCase _ _ _ _ _ (AnyCodeIn l)) receivedCode
  | receivedCode `notElem` l = Just $ StatusFailure smokeCase receivedCode
  | otherwise = Nothing
