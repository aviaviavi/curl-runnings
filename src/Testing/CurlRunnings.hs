{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | curl-runnings is a framework for writing declaratively writing curl based tests for your API's.
-- Write your test specifications with yaml or json, and you're done!
module Testing.CurlRunnings
  ( runCase
  , runSuite
  , decodeFile
  ) where

import           Control.Exception
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
import qualified Data.Text.Encoding                   as T
import qualified Data.Vector                          as V
import qualified Data.Yaml.Include                    as YI
import           Network.Connection                   (TLSSettings (..))
import           Network.HTTP.Client.TLS              (mkManagerSettings)
import           Network.HTTP.Conduit
import           Network.HTTP.Simple                  hiding (Header)
import qualified Network.HTTP.Simple                  as HTTP
import qualified Network.HTTP.Types                   as NT
import           System.Directory
import           System.Environment
import           Testing.CurlRunnings.Internal
import           Testing.CurlRunnings.Internal.Parser
import           Testing.CurlRunnings.Types
import           Text.Printf
import           Text.Regex.Posix


-- | decode a json or yaml file into a suite object
decodeFile :: FilePath -> IO (Either String CurlSuite)
decodeFile specPath =
  doesFileExist specPath >>= \exists ->
    if exists
      then case last $ T.splitOn "." (T.pack specPath) of
             "json" ->
               eitherDecode' <$> B.readFile specPath :: IO (Either String CurlSuite)
             "yaml" -> mapLeft show <$> YI.decodeFileEither specPath
             "yml" -> mapLeft show <$> YI.decodeFileEither specPath
             _ -> return . Left $ printf "Invalid spec path %s" specPath
      else return . Left $ printf "%s not found" specPath


noVerifyTlsManagerSettings :: ManagerSettings
noVerifyTlsManagerSettings = mkManagerSettings noVerifyTlsSettings Nothing

noVerifyTlsSettings :: TLSSettings
noVerifyTlsSettings =
  TLSSettingsSimple
  { settingDisableCertificateValidation = True
  , settingDisableSession = True
  , settingUseServerName = False
  }

-- | Fetch existing query parameters from the request and append those specfied in the queryParameters field.
appendQueryParameters :: [QueryParameter] -> Request -> Request
appendQueryParameters newParams r = setQueryString (existing ++ newQuery) r where
  existing = NT.parseQuery $ queryString r
  newQuery = NT.simpleQueryToQuery $ fmap (\(QueryParameter k v) -> (T.encodeUtf8 k, T.encodeUtf8 v)) newParams

-- | Run a single test case, and returns the result. IO is needed here since this method is responsible
-- for actually curling the test case endpoint and parsing the result.
runCase :: CurlRunningsState -> CurlCase -> IO CaseResult
runCase state@(CurlRunningsState _ _ _ tlsCheckType) curlCase = do
  let eInterpolatedUrl = interpolateQueryString state $ url curlCase
      eInterpolatedHeaders =
        interpolateHeaders state $ fromMaybe (HeaderSet []) (headers curlCase)
      eInterpolatedQueryParams = interpolateViaJSON state $ fromMaybe (QueryParameters []) (queryParameters curlCase)
  case (eInterpolatedUrl, eInterpolatedHeaders, eInterpolatedQueryParams) of
    (Left err, _, _) ->
      return $ CaseFail curlCase Nothing Nothing [QueryFailure curlCase err]
    (_, Left err, _) ->
      return $ CaseFail curlCase Nothing Nothing [QueryFailure curlCase err]
    (_, _, Left err) ->
      return $ CaseFail curlCase Nothing Nothing [QueryFailure curlCase err]
    (Right interpolatedUrl, Right interpolatedHeaders, Right (QueryParameters interpolatedQueryParams)) ->
      case sequence $ runReplacements state <$> requestData curlCase of
        Left l ->
          return $ CaseFail curlCase Nothing Nothing [QueryFailure curlCase l]
        Right replacedJSON -> do
          initReq <- parseRequest $ T.unpack interpolatedUrl
          manager <- newManager noVerifyTlsManagerSettings

          let request =
                setRequestBodyJSON (fromMaybe emptyObject replacedJSON) .
                setRequestHeaders (toHTTPHeaders interpolatedHeaders) .
                appendQueryParameters interpolatedQueryParams  .
                (if tlsCheckType == DoTLSCheck then id else (setRequestManager manager)) $
                initReq {method = B8S.pack . show $ requestMethod curlCase}

          logger state DEBUG (pShow request)
          logger
            state
            DEBUG
            ("Request body: " <> (pShow $ fromMaybe emptyObject replacedJSON))
          response <- httpBS request
          -- If the response is just returning bytes, we won't print them
          let responseHeaderValues = map snd (getResponseHeaders response)
          if "application/octet-stream" `notElem` responseHeaderValues &&
             "application/vnd.apple.pkpass" `notElem` responseHeaderValues
            then catch
                   (logger state DEBUG (pShow response))
                   (\(e :: IOException) ->
                      logger state ERROR ("Error logging response: " <> pShow e))
              -- TODO: we should log as much info as possible without printing the raw body
            else logger
                   state
                   DEBUG
                   "Response output supressed (returned content-type was bytes)"
          returnVal <-
            catch
              ((return . decode . B.fromStrict $ getResponseBody response) :: IO (Maybe Value))
              (\(e :: IOException) ->
                 logger
                   state
                   ERROR
                   ("Error decoding response into json: " <> pShow e) >>
                 return (Just Null))
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
              [] -> CasePass curlCase (Just receivedHeaders) returnVal
              failures ->
                CaseFail curlCase (Just receivedHeaders) returnVal failures

checkHeaders ::
     CurlRunningsState -> CurlCase -> Headers -> Maybe AssertionFailure
checkHeaders _ (CurlCase _ _ _ _ _ _ _ _ Nothing) _ = Nothing
checkHeaders state curlCase@(CurlCase _ _ _ _ _ _ _ _ (Just (HeaderMatcher m))) receivedHeaders =
  let interpolatedHeaders = mapM (interpolatePartialHeader state) m
  in case interpolatedHeaders of
       Left f -> Just $ QueryFailure curlCase f
       Right headerList ->
         let notFound =
               filter
                 (not . headerIn receivedHeaders)
                 (unsafeLogger state DEBUG "header matchers" headerList)
         in if null notFound
              then Nothing
              else Just $
                   HeaderFailure
                     curlCase
                     (HeaderMatcher headerList)
                     receivedHeaders

interpolatePartialHeader ::
     CurlRunningsState
  -> PartialHeaderMatcher
  -> Either QueryError PartialHeaderMatcher
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
         unsafeLogger state ERROR "WARNING: empty header matcher found" . Right $
         PartialHeaderMatcher Nothing Nothing

interpolateHeaders :: CurlRunningsState -> Headers -> Either QueryError Headers
interpolateHeaders state (HeaderSet headerList) =
  mapM
    (\(Header k v) ->
       case sequence
              [interpolateQueryString state k, interpolateQueryString state v] of
         Left err       -> Left err
         Right [k', v'] -> Right $ Header k' v')
    headerList >>=
  (Right . HeaderSet)

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
runSuite :: CurlSuite -> LogLevel -> TLSCheckType -> IO [CaseResult]
runSuite (CurlSuite cases filterRegex) logLevel tlsType = do
  fullEnv <- getEnvironment
  let envMap = H.fromList $ map (\(x, y) -> (T.pack x, T.pack y)) fullEnv
      filterNameByRegexp curlCase =
        maybe
          True
          (\regexp -> T.unpack (name curlCase) =~ T.unpack regexp :: Bool)
          filterRegex
  foldM
    (\prevResults curlCase ->
       case safeLast prevResults of
         Just CaseFail {} -> return prevResults
         Just CasePass {} -> do
           result <-
             runCase (CurlRunningsState envMap prevResults logLevel tlsType) curlCase >>=
             printR
           return $ prevResults ++ [result]
         Nothing -> do
           result <-
             runCase (CurlRunningsState envMap [] logLevel tlsType) curlCase >>= printR
           return [result])
    []
    (filter filterNameByRegexp cases)

-- | Check if the retrieved value fail's the case's assertion
checkBody ::
     CurlRunningsState -> CurlCase -> Maybe Value -> Maybe AssertionFailure
-- | We are looking for an exact payload match, and we have a payload to check
checkBody state curlCase@(CurlCase _ _ _ _ _ _ (Just (Exactly expectedValue)) _ _) (Just receivedBody) =
  case runReplacements state expectedValue of
    (Left err) -> Just $ QueryFailure curlCase err
    (Right interpolated) ->
      if (unsafeLogger state DEBUG "exact body matcher" interpolated) /=
         receivedBody
        then Just $
             DataFailure
               (curlCase {expectData = Just $ Exactly interpolated})
               (Exactly interpolated)
               (Just receivedBody)
        else Nothing
-- | We are checking a list of expected subvalues, and we have a payload to check
checkBody state curlCase@(CurlCase _ _ _ _ _ _ (Just (Contains subexprs)) _ _) (Just receivedBody) =
  case runReplacementsOnSubvalues state subexprs of
    Left f -> Just $ QueryFailure curlCase f
    Right updatedMatcher ->
      if jsonContainsAll
           receivedBody
           (unsafeLogger state DEBUG "partial json body matcher" updatedMatcher)
        then Nothing
        else Just $
             DataFailure curlCase (Contains updatedMatcher) (Just receivedBody)
-- | We are checking a list of expected absent subvalues, and we have a payload to check
checkBody state curlCase@(CurlCase _ _ _ _ _ _ (Just (NotContains subexprs)) _ _) (Just receivedBody) =
  case runReplacementsOnSubvalues state subexprs of
    Left f -> Just $ QueryFailure curlCase f
    Right updatedMatcher ->
      if jsonContainsAny
           receivedBody
           (unsafeLogger state DEBUG "partial json body matcher" updatedMatcher)
        then Just $
             DataFailure
               curlCase
               (NotContains updatedMatcher)
               (Just receivedBody)
        else Nothing
-- | We are checking for both contains and notContains vals, and we have a payload to check
checkBody state curlCase@(CurlCase _ _ _ _ _ _ (Just m@(MixedContains subexprs)) _ _) receivedBody =
  let failure =
        join $
        find
          isJust
          (map
             (\subexpr ->
                checkBody
                  state
                  curlCase {expectData = Just subexpr}
                  receivedBody)
             subexprs)
  in fmap (\_ -> DataFailure curlCase m receivedBody) failure
-- | We expected a body but didn't get one
checkBody _ curlCase@(CurlCase _ _ _ _ _ _ (Just anything) _ _) Nothing =
  Just $ DataFailure curlCase anything Nothing
-- | No assertions on the body
checkBody _ (CurlCase _ _ _ _ _ _ Nothing _ _) _ = Nothing

runReplacementsOnSubvalues ::
     CurlRunningsState -> [JsonSubExpr] -> Either QueryError [JsonSubExpr]
runReplacementsOnSubvalues state =
  mapM
    (\expr ->
       case expr of
         ValueMatch v ->
           case runReplacements state v of
             Left l       -> Left l
             Right newVal -> Right $ ValueMatch newVal
         KeyMatch k ->
           case interpolateQueryString state k of
             Left l       -> Left l
             Right newKey -> Right $ KeyMatch newKey
         KeyValueMatch k v ->
           case (interpolateQueryString state k, runReplacements state v) of
             (Left l, _) -> Left l
             (_, Left l) -> Left l
             (Right k', Right v') ->
               Right KeyValueMatch {matchKey = k', matchValue = v'})

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
  let results = V.mapM (runReplacements p) a
  in case results of
       Left l  -> Left l
       Right r -> Right $ Array r
-- special case, i can't figure out how to get the parser to parse empty strings :'(
runReplacements _ s@(String "") = Right s
runReplacements state (String s) =
  case parseQuery s of
    Right [LiteralText t] -> Right $ String t
    Right [q@(InterpolatedQuery _ _)] ->
      getStringValueForQuery state q >>= (Right . String)
    Right [q@(NonInterpolatedQuery _)] -> getValueForQuery state q
    Right _ -> mapRight String $ interpolateQueryString state s
    Left parseErr -> Left parseErr
runReplacements _ valToUpdate = Right valToUpdate

-- | Given an instance of both ToJSON and FromJSON return a new instance with
-- interpolated values.
-- NB: interpolateViaJSON assumes that fromJSON . toJSON is identity
interpolateViaJSON :: (ToJSON a, FromJSON a) => CurlRunningsState -> a -> Either QueryError a
interpolateViaJSON state i = do
  replaced <- runReplacements state $ toJSON i
  resultToEither $ fromJSON replaced where
    resultToEither (Error e)   = Left $ QueryValidationError $ T.pack e
    resultToEither (Success a) = Right a

-- | Given a query string, return some text with interpolated values. Type
-- errors will be returned if queries don't resolve to strings
interpolateQueryString ::
     CurlRunningsState -> FullQueryText -> Either QueryError T.Text
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
getStringValueForQuery ::
     CurlRunningsState -> InterpolatedQuery -> Either QueryError T.Text
getStringValueForQuery _ (LiteralText rawText) = Right rawText
getStringValueForQuery state (NonInterpolatedQuery q) =
  getStringValueForQuery state $ InterpolatedQuery "" q
getStringValueForQuery state i@(InterpolatedQuery rawText (Query _)) =
  case getValueForQuery state i of
    Left l           -> Left l
    Right (String s) -> Right $ rawText <> s
    (Right o)        -> Left $ QueryTypeMismatch "Expected a string" o
getStringValueForQuery (CurlRunningsState env _ _ _) (InterpolatedQuery rawText (EnvironmentVariable v)) =
  Right $ rawText <> H.lookupDefault "" v env

-- | Lookup the value for the specified query
getValueForQuery ::
     CurlRunningsState -> InterpolatedQuery -> Either QueryError Value
getValueForQuery _ (LiteralText rawText) = Right $ String rawText
getValueForQuery (CurlRunningsState _ previousResults _ _) full@(NonInterpolatedQuery (Query indexes)) =
  case head indexes of
    (CaseResultIndex i) ->
      let maybeCase = arrayGet previousResults $ fromInteger i
      in if isJust maybeCase
           then let (CasePass _ _ returnedJSON) = fromJust maybeCase
                    jsonToIndex =
                      case returnedJSON of
                        Just v -> Right v
                        Nothing ->
                          Left $
                          NullPointer
                            (T.pack $ show full)
                            "No data was returned from this case"
                in foldl
                     (\eitherVal index ->
                        case (eitherVal, index) of
                          (Left l, _) -> Left l
                          (Right (Object o), KeyIndex k) ->
                            Right $ H.lookupDefault Null k o
                          (Right (Array a), ArrayIndex i') ->
                            maybe
                              (Left $
                               NullPointer (T.pack $ show full) $
                               "Array index not found: " <> T.pack (show i'))
                              Right
                              (arrayGet (V.toList a) $ fromInteger i')
                          (Right Null, q) ->
                            Left $
                            NullPointer (T.pack $ show full) (T.pack $ show q)
                          (Right o, _) ->
                            Left $ QueryTypeMismatch (T.pack $ show index) o)
                     jsonToIndex
                     (tail indexes)
           else Left $
                NullPointer (T.pack $ show full) $
                "Attempted to index into previous a test case that didn't exist: " <>
                T.pack (show i)
    _ ->
      Left . QueryValidationError $
      T.pack $
      "'$< ... >' queries must start with a RESPONSES[index] query: " ++
      show full
getValueForQuery (CurlRunningsState env _ _ _) (NonInterpolatedQuery (EnvironmentVariable var)) =
  Right . String $ H.lookupDefault "" var env
getValueForQuery state (InterpolatedQuery _ q) =
  case getValueForQuery state (NonInterpolatedQuery q) of
    Right (String s) -> Right . String $ s
    Right Null -> Right Null
    Right v -> Left $ QueryTypeMismatch (T.pack "Expected a string") v
    Left l -> Left l

jsonContains ::
     ((JsonSubExpr -> Bool) -> [JsonSubExpr] -> Bool)
  -> Value
  -> [JsonSubExpr]
  -> Bool
jsonContains f jsonValue =
  let traversedValue = traverseValue jsonValue
  in f $ \match ->
       case match of
         ValueMatch subval -> subval `elem` traversedValue
         KeyMatch key -> any (`containsKey` key) traversedValue
         KeyValueMatch key subval ->
           any (\o -> containsKeyVal o key subval) traversedValue

-- | Does the json value contain all of these sub-values?
jsonContainsAll :: Value -> [JsonSubExpr] -> Bool
jsonContainsAll = jsonContains all

-- | Does the json value contain any of these sub-values?
jsonContainsAny :: Value -> [JsonSubExpr] -> Bool
jsonContainsAny = jsonContains any

-- | Does the json value contain the given key value pair?
containsKeyVal :: Value -> T.Text -> Value -> Bool
containsKeyVal jsonValue key val =
  case jsonValue of
    Object o -> H.lookup key o == Just val
    _        -> False

-- | Does the json value contain the given key value pair?
containsKey :: Value -> T.Text -> Bool
containsKey jsonValue key =
  case jsonValue of
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
checkCode curlCase@(CurlCase _ _ _ _ _ _ _ (ExactCode expectedCode) _) receivedCode
  | expectedCode /= receivedCode = Just $ StatusFailure curlCase receivedCode
  | otherwise = Nothing
checkCode curlCase@(CurlCase _ _ _ _ _ _ _ (AnyCodeIn l) _) receivedCode
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
toHTTPHeader (Header a b) =
  (CI.mk . B8S.pack $ T.unpack a, B8S.pack $ T.unpack b)

-- | Utility conversion from CurlRunnings headers to HTTP headers.
toHTTPHeaders :: Headers -> HTTP.RequestHeaders
toHTTPHeaders (HeaderSet h) = map toHTTPHeader h
