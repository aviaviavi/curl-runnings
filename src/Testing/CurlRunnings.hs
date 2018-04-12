{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.Aeson.Types
import qualified Data.ByteString.Char8                as B8S
import qualified Data.ByteString.Lazy                 as B
import qualified Data.CaseInsensitive                 as CI
import           Data.Either
import qualified Data.HashMap.Strict                  as H
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Vector                          as V
import qualified Data.Yaml                            as Y
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import qualified Network.HTTP.Types.Header            as HTTP
import           Testing.CurlRunnings.Internal
import           Testing.CurlRunnings.Internal.Parser
import           Testing.CurlRunnings.Types
import           Text.Printf
import           System.Directory
import           System.Environment

-- | decode a json or yaml file into a suite object
decodeFile :: FilePath -> IO (Either String CurlSuite)
decodeFile specPath = doesFileExist specPath >>= \exists ->
  if exists then
    case last $ T.splitOn "." (T.pack specPath) of
      "json" ->
        eitherDecode' <$> B.readFile specPath :: IO (Either String CurlSuite)
      "yaml" ->
        Y.decodeEither <$> B8S.readFile specPath :: IO (Either String CurlSuite)
      "yml" ->
        Y.decodeEither <$> B8S.readFile specPath :: IO (Either String CurlSuite)
      _ -> return . Left $ printf "Invalid spec path %s" specPath
  else return . Left $ printf "%s not found" specPath

-- | Run a single test case, and returns the result. IO is needed here since this method is responsible
-- for actually curling the test case endpoint and parsing the result.
runCase :: CurlRunningsState -> CurlCase -> IO CaseResult
runCase state curlCase = do
  let eInterpolatedUrl = interpolateQueryString state $ T.pack $ url curlCase
  case eInterpolatedUrl of
    Left err -> return $ CaseFail curlCase Nothing Nothing [QueryFailure curlCase err]
    Right interpolatedUrl -> do
      initReq <- parseRequest $ T.unpack interpolatedUrl
      response <-
        httpBS .
        setRequestBodyJSON (fromMaybe emptyObject (requestData curlCase)) .
        setRequestHeaders
          (toHTTPHeaders $ fromMaybe (HeaderSet []) (headers curlCase)) $
        initReq {method = B8S.pack . show $ requestMethod curlCase}
      returnVal <-
        (return . decode . B.fromStrict $ getResponseBody response) :: IO (Maybe Value)
      let returnCode = getResponseStatusCode response
          receivedHeaders = fromHTTPHeaders $ responseHeaders response
          assertionErrors =
            map fromJust $
            filter
              isJust
              [ checkBody state curlCase returnVal
              , checkCode curlCase returnCode
              , checkHeaders state curlCase receivedHeaders
              ]
      return $
        case assertionErrors of
          []       -> CasePass curlCase (Just receivedHeaders) returnVal
          failures -> CaseFail curlCase (Just receivedHeaders) returnVal failures

checkHeaders :: CurlRunningsState -> CurlCase -> Headers -> Maybe AssertionFailure
checkHeaders _ (CurlCase _ _ _ _ _ _ _ Nothing) _ = Nothing
checkHeaders state curlCase@(CurlCase _ _ _ _ _ _ _ (Just matcher@(HeaderMatcher m))) receivedHeaders =
  let interpolatedHeaderAttempts = map (interpolatePartialHeader state) m
  in case find isLeft interpolatedHeaderAttempts of
       Just (Left f) -> Just $ QueryFailure curlCase f
       _ ->
         let successfulInterpolations =
               map
                 (fromRight
                    (error
                       $ programCrashString "checkHeaders"))
                 interpolatedHeaderAttempts
             notFound =
               filter (not . headerIn receivedHeaders) successfulInterpolations
         in if null notFound
              then Nothing
              else Just $
                   HeaderFailure
                     curlCase
                     (HeaderMatcher successfulInterpolations)
                     receivedHeaders

interpolatePartialHeader :: CurlRunningsState -> PartialHeaderMatcher -> Either QueryError PartialHeaderMatcher
interpolatePartialHeader state (PartialHeaderMatcher k v) =
  let k' = interpolateQueryString state <$> k
      v' = interpolateQueryString state <$> v
  in case (k', v') of
       (Just (Left err), _) -> Left err
       (_, Just (Left err)) -> Left err
       (Just (Right p), Just (Right q)) ->
         Right $ PartialHeaderMatcher (Just p) (Just q)
       (Just (Right p), Nothing) ->
         Right $ PartialHeaderMatcher (Just p) Nothing
       (Nothing, Just (Right p)) ->
         Right $ PartialHeaderMatcher Nothing (Just p)
       _ ->
         tracer "WARNING: empty header matcher found" . Right $
         PartialHeaderMatcher Nothing Nothing

-- | Does this header contain our matcher?
headerMatches :: PartialHeaderMatcher -> Header -> Bool
headerMatches (PartialHeaderMatcher mk mv) (Header k v) =
  maybe True (== k) mk && maybe True (== v) mv

-- | Does any of these headers contain our matcher?
headerIn :: Headers -> PartialHeaderMatcher -> Bool
headerIn (HeaderSet received) headerMatcher =
  any (headerMatches headerMatcher) received

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x  = Just $ last x

printR :: Show a => a -> IO a
printR x = print x >> return x

-- | Runs the test cases in order and stop when an error is hit. Returns all the results
runSuite :: CurlSuite -> IO [CaseResult]
runSuite (CurlSuite cases) = do
  fullEnv <- getEnvironment
  let envMap = H.fromList $ map (\(x, y) -> (T.pack x, T.pack y)) fullEnv
  foldM
    (\prevResults curlCase ->
       case safeLast prevResults of
         Just CaseFail {} -> return prevResults
         Just CasePass {} -> do
           result <- runCase (CurlRunningsState envMap prevResults) curlCase >>= printR
           return $ prevResults ++ [result]
         Nothing -> do
           result <- runCase (CurlRunningsState envMap []) curlCase >>= printR
           return [result])
    []
    cases

-- | Check if the retrieved value fail's the case's assertion
checkBody :: CurlRunningsState -> CurlCase -> Maybe Value -> Maybe AssertionFailure
-- | We are looking for an exact payload match, and we have a payload to check
checkBody state curlCase@(CurlCase _ _ _ _ _ (Just (Exactly expectedValue)) _ _) (Just receivedBody) =
  case runReplacements state expectedValue of
    (Left err) -> Just $ QueryFailure curlCase err
    (Right interpolated) ->
      if interpolated /= receivedBody
        then Just $
             DataFailure
               (curlCase {expectData = Just $ Exactly interpolated})
               (Exactly interpolated)
               (Just receivedBody)
        else Nothing

-- | We are checking a list of expected subvalues, and we have a payload to check
checkBody state curlCase@(CurlCase _ _ _ _ _ (Just (Contains subexprs)) _ _) (Just receivedBody) =
  case runReplacementsOnSubvalues state subexprs of
    Left f -> Just $ QueryFailure curlCase f
    Right updatedMatcher ->
      if jsonContainsAll receivedBody updatedMatcher
        then Nothing
        else Just $
             DataFailure curlCase (Contains updatedMatcher) (Just receivedBody)

-- | We expected a body but didn't get one
checkBody _ curlCase@(CurlCase _ _ _ _ _ (Just anything) _ _) Nothing =
  Just $ DataFailure curlCase anything Nothing

-- | No assertions on the body
checkBody _ (CurlCase _ _ _ _ _ Nothing _ _) _ = Nothing

runReplacementsOnSubvalues :: CurlRunningsState -> [JsonSubExpr] -> Either QueryError [JsonSubExpr]
runReplacementsOnSubvalues state subexprs =
  let replacementResults :: [Either QueryError JsonSubExpr] =
        map
          (\expr ->
             case expr of
               ValueMatch v ->
                 case runReplacements state v of
                   Left l -> Left l
                   Right newVal -> Right $ ValueMatch newVal
               KeyValueMatch k v ->
                 case (interpolateQueryString state k, runReplacements state v) of
                   (Left l, _) -> Left l
                   (_, Left l) -> Left l
                   (Right k', Right v') ->
                     Right KeyValueMatch {matchKey = k', matchValue = v'})
          subexprs
  in case find isLeft replacementResults of
       Just (Left err) -> Left err
       Nothing ->
         Right $
         map
           (fromRight
              (error
                 $ programCrashString "runReplacementsOnSubvalues"))
           replacementResults

-- | runReplacements
runReplacements :: CurlRunningsState -> Value -> Either QueryError Value
runReplacements state (Object o) =
  let keys = H.keys o
      keysWithUpdatedKeyVal =
        map
          (\key ->
             let value = fromJust $ H.lookup key o
              -- (old key, new key, new value)
             in ( key
                , interpolateQueryString state key
                , runReplacements state value))
          keys
  in mapRight Object $
     foldr
       (\((key, eKeyResult, eValueResult) :: ( T.Text
                                             , Either QueryError T.Text
                                             , Either QueryError Value)) (eObjectToUpdate :: Either QueryError Object) ->
          case (eKeyResult, eValueResult, eObjectToUpdate)
            -- TODO there should be a more elegant way to write this error
            -- handling below
                of
            (Left queryErr, _, _) -> Left queryErr
            (_, Left queryErr, _) -> Left queryErr
            (_, _, Left queryErr) -> Left queryErr
            (Right newKey, Right newValue, Right objectToUpdate) ->
              if key /= newKey
                then let inserted = H.insert newKey newValue objectToUpdate
                         deleted = H.delete key inserted
                     in Right deleted
                else Right $ H.insert key newValue objectToUpdate)
       (Right o)
       keysWithUpdatedKeyVal
runReplacements p (Array a) =
  let results = V.map (runReplacements p) a
  in case find isLeft results of
       Just l -> l
       Nothing ->
         Right . Array $
         V.map
           (fromRight
              (error
                 $ programCrashString "runReplacements"))
           results
-- special case, i can't figure out how to get the parser to parse empty strings :'(
runReplacements _ s@(String "") = Right s
runReplacements state (String s) =
  case parseQuery s of
    Right [LiteralText t] -> Right $ String t
    Right [q@(InterpolatedQuery _ _)] -> getValueForQuery state q
    Right [q@(NonInterpolatedQuery _)] -> getValueForQuery state q
    Right _ -> mapRight String $ interpolateQueryString state s
    Left parseErr -> Left parseErr
runReplacements _ valToUpdate = Right valToUpdate

-- | Given a query string, return some text with interpolated values. Type
-- errors will be returned if queries don't resolve to strings
interpolateQueryString :: CurlRunningsState -> FullQueryText -> Either QueryError T.Text
interpolateQueryString state query =
  let parsedQuery = parseQuery query
  in case parsedQuery of
       (Left err) -> Left err
       (Right interpolatedQ) ->
         let lookups :: [Either QueryError T.Text] =
               map (getStringValueForQuery state) interpolatedQ
             failure = find isLeft lookups
             goodLookups :: [T.Text] =
               Prelude.map (fromRight (T.pack "error")) lookups
         in fromMaybe (Right $ foldr (<>) (T.pack "") goodLookups) failure

-- | Lookup the text at the specified query
getStringValueForQuery :: CurlRunningsState -> InterpolatedQuery -> Either QueryError T.Text
getStringValueForQuery _ (LiteralText rawText) = Right rawText
getStringValueForQuery state (NonInterpolatedQuery q) =
  getStringValueForQuery state $ InterpolatedQuery "" q
getStringValueForQuery state i@(InterpolatedQuery rawText (Query _)) =
  case getValueForQuery state i of
    Left l           -> Left l
    Right (String s) -> Right $ rawText <> s
    (Right o)        -> Left $ QueryTypeMismatch "Expected a string" o
getStringValueForQuery (CurlRunningsState env _) (InterpolatedQuery rawText (EnvironmentVariable v)) = Right $ rawText <> H.lookupDefault "" v env

-- | Lookup the value for the specified query
getValueForQuery :: CurlRunningsState -> InterpolatedQuery -> Either QueryError Value
getValueForQuery _ (LiteralText rawText) = Right $ String rawText
getValueForQuery (CurlRunningsState _ previousResults) full@(NonInterpolatedQuery (Query indexes)) =
  case head indexes of
    (CaseResultIndex i) ->
      let (CasePass _ _ returnedJSON) = previousResults !! fromInteger i
          jsonToIndex =
            case returnedJSON of
              Just v -> Right v
              Nothing ->
                Left $
                NullPointer
                  (T.pack $ show full)
                  "No data was returned from this case"
      in foldr
           (\index eitherVal ->
              case (eitherVal, index) of
                (Left l, _) -> Left l
                (Right (Object o), KeyIndex k) ->
                  Right $ H.lookupDefault Null k o
                (Right (Array a), ArrayIndex i') -> Right $ a V.! fromInteger i'
                (Right Null, q) ->
                  Left $ NullPointer (T.pack $ show full) (T.pack $ show q)
                (Right o, _) -> Left $ QueryTypeMismatch (T.pack $ show index) o)
           jsonToIndex
           (tail indexes)
    _ ->
      Left . QueryValidationError $
      T.pack $ "$<> queries must start with a SUITE[index] query: " ++ show full
getValueForQuery (CurlRunningsState env _) (NonInterpolatedQuery (EnvironmentVariable var)) =
  Right . String $ H.lookupDefault "" var env
getValueForQuery state (InterpolatedQuery _ q) =
  case getValueForQuery state (NonInterpolatedQuery q) of
    Right (String s) -> Right . String $ s
    Right Null -> Right Null
    Right v -> Left $ QueryTypeMismatch (T.pack "Expected a string") v
    Left l -> Left l

-- | Does the json value contain all of these sub-values?
jsonContainsAll :: Value -> [JsonSubExpr] -> Bool
jsonContainsAll jsonValue =
  all $ \match ->
    case match of
      ValueMatch subval -> subval `elem` traverseValue jsonValue
      KeyValueMatch key subval ->
        any (\o -> containsKeyVal o key subval) (traverseValue jsonValue)

-- | Does the json value contain the given key value pair?
containsKeyVal :: Value -> T.Text -> Value -> Bool
containsKeyVal jsonValue key val = case jsonValue of
  Object o -> H.lookup key o == Just val
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
checkCode curlCase@(CurlCase _ _ _ _ _ _ (ExactCode expectedCode) _) receivedCode
  | expectedCode /= receivedCode = Just $ StatusFailure curlCase receivedCode
  | otherwise = Nothing
checkCode curlCase@(CurlCase _ _ _ _ _ _ (AnyCodeIn l) _) receivedCode
  | receivedCode `notElem` l = Just $ StatusFailure curlCase receivedCode
  | otherwise = Nothing

-- | Utility conversion from HTTP headers to CurlRunnings headers.
fromHTTPHeaders :: HTTP.ResponseHeaders -> Headers
fromHTTPHeaders rh = HeaderSet $ map fromHTTPHeader rh

-- | Utility conversion from an HTTP header to a CurlRunnings header.
fromHTTPHeader :: HTTP.Header -> Header
fromHTTPHeader (a, b) =
  Header (T.pack . B8S.unpack $ CI.original a) (T.pack $ B8S.unpack b)

-- | Utility conversion from an HTTP header to a CurlRunnings header.
toHTTPHeader :: Header -> HTTP.Header
toHTTPHeader (Header a b) = (CI.mk . B8S.pack $ T.unpack a, B8S.pack $ T.unpack b)

-- | Utility conversion from CurlRunnings headers to HTTP headers.
toHTTPHeaders :: Headers -> HTTP.RequestHeaders
toHTTPHeaders (HeaderSet h) = map toHTTPHeader h

-- | TODO - we should refactor to get rid of this
programCrashString :: T.Text -> String
programCrashString = T.unpack . ("curl runnings crashed to due a bug! Tried to unpack an Either value that turned out to be a Left in: " <>)
