{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}

module Main where

import           Data.Aeson
import           Data.ByteString                             (ByteString)
import           Data.Either
import qualified Data.Text                                   as T
import           System.Directory
import           Test.Hspec
import           Testing.CurlRunnings
import           Testing.CurlRunnings.Internal
import qualified Testing.CurlRunnings.Internal.Aeson         as A
import           Testing.CurlRunnings.Internal.Headers
import           Testing.CurlRunnings.Internal.KeyValuePairs
import           Testing.CurlRunnings.Internal.Parser
import           Testing.CurlRunnings.Internal.Payload
import           Text.RawString.QQ

main :: IO ()
main = hspec $
  describe "Curl Runnings" $ do
  it "should provide valid example yaml specs" $
    testValidSpec "/examples/example-spec.yaml"

  it "should provide valid example json specs" $
    testValidSpec "/examples/example-spec.json"

  it "should provide valid example dhall specs" $
    testValidSpec "/examples/example-spec.dhall"

  -- note that this doesn't actually try to parse the interpolations themselves,
  -- but that would be useful thing to add here
  it "should provid a valid interpolation spec" $
    testValidSpec "/examples/interpolation-spec.yaml"

  it "should parse valid queries" $ do
    parseQuery "just some text" `shouldSatisfy` isRight
    parseQuery "$<RESPONSES[0].key.key>" `shouldSatisfy` isRight
    parseQuery "$<RESPONSES[0].key.key[0].key_with_underscores>" `shouldSatisfy` isRight
    parseQuery "$<RESPONSES[0].key.key[0]._key_starts_with_underscores>" `shouldSatisfy` isRight
    parseQuery "$<RESPONSES[100].key.key[0].key_with_underscores>" `shouldSatisfy` isRight
    parseQuery "some text before $<RESPONSES[100].key.key[0].key_with_underscores> and after" `shouldSatisfy` isRight
    parseQuery "some $<RESPONSES[100]> querires $<RESPONSES[100]>" `shouldSatisfy` isRight
    parseQuery "some $<RESPONSES[100]> querires $<RESPONSES[100]> ${SOME_ENV_VARIABLE} asdf" `shouldSatisfy` isRight
    parseQuery "$<SUITE[0].key.key>" `shouldSatisfy` isRight -- legacy SUITE should still be supported

  it "should reject invalid queries" $ do
    parseQuery "$<" `shouldSatisfy` isLeft
    parseQuery "$<RESPONSES[f].key>" `shouldSatisfy` isLeft
    parseQuery "$<RESPONSES[1].key[r]>" `shouldSatisfy` isLeft
    parseQuery "$<RESPONSES[1].key[1][1] $<>>" `shouldSatisfy` isLeft
    parseQuery "$<RESPONSES[1].key[1][1]" `shouldSatisfy` isLeft
    parseQuery "$<RESPONSES[1]>.key[1][1] ${" `shouldSatisfy` isLeft
    parseQuery "$<POOP[0].key.key>" `shouldSatisfy` isLeft
    parseQuery "some text $<BAD_RESPONSES_REF[0].key.key>" `shouldSatisfy` isLeft

  it "should parse valid headers" $ do
    parseHeaders "key:" `shouldBeHeaders` [("key", "")]
    parseHeaders "key: ;" `shouldBeHeaders` [("key", "")]
    parseHeaders "key:" `shouldBeHeaders` [("key", "")]
    parseHeaders "key: value" `shouldBeHeaders` [("key", "value")]
    parseHeaders "key: value;" `shouldBeHeaders` [("key", "value")]
    parseHeaders "key : value" `shouldBeHeaders` [("key", "value")]
    parseHeaders "key :value" `shouldBeHeaders` [("key", "value")]
    parseHeaders "key : value " `shouldBeHeaders` [("key", "value")]
    parseHeaders "  key : value " `shouldBeHeaders` [("key", "value")]
    parseHeaders "key name : value" `shouldBeHeaders` [("key name", "value")]
    parseHeaders "key : value name " `shouldBeHeaders` [("key", "value name")]
    parseHeaders "key1 : value1:value2;" `shouldBeHeaders` [("key1", "value1:value2")]
    parseHeaders "key1 : value1:value2" `shouldBeHeaders` [("key1", "value1:value2")]
    parseHeaders "key1 : value1 ; key2: value2" `shouldBeHeaders` [("key1", "value1"), ("key2", "value2")]
    parseHeaders "key1 : value1 ;\n key2: value2" `shouldBeHeaders` [("key1", "value1"), ("key2", "value2")]
    parseHeaders "key1 : value1 ;\n key2: value2\n" `shouldBeHeaders` [("key1", "value1"), ("key2", "value2")]
    parseHeaders "key1 : value1 ;\n key2: value2;" `shouldBeHeaders` [("key1", "value1"), ("key2", "value2")]
    parseHeaders "Content-Type : application/json" `shouldBeHeaders` [("Content-Type", "application/json")]
    parseHeaders "$<RESPONSES[1].key[r]>: $<RESPONSES[100]> ;" `shouldBeHeaders` [("$<RESPONSES[1].key[r]>", "$<RESPONSES[100]>")]

  it "should reject invalid headers" $ do
    parseHeaders "" `shouldSatisfy` isLeft
    parseHeaders "\n" `shouldSatisfy` isLeft
    parseHeaders "£key: value" `shouldSatisfy` isLeft
    parseHeaders "key: £value" `shouldSatisfy` isLeft
    parseHeaders "key" `shouldSatisfy` isLeft
    parseHeaders "key ;" `shouldSatisfy` isLeft
    parseHeaders " : value" `shouldSatisfy` isLeft
    parseHeaders " : value ;" `shouldSatisfy` isLeft
    parseHeaders " : value ; key : value2" `shouldSatisfy` isLeft

  it "arrayGet should handle positive and negative indexes correctly" $ do
    let a = [1, 2, 3]
        b = [] :: [Int]
    (arrayGet a 0) `shouldBe` Just (1 :: Integer)
    (arrayGet a 1) `shouldBe` Just (2 :: Integer)
    (arrayGet a 2) `shouldBe` Just (3 :: Integer)
    (arrayGet a (-1)) `shouldBe` Just (3 :: Integer)
    (arrayGet a (-2)) `shouldBe` Just (2 :: Integer)
    (arrayGet a (-3)) `shouldBe` Just (1 :: Integer)
    (arrayGet a (-4)) `shouldBe` Nothing
    (arrayGet a 3) `shouldBe` Nothing
    (arrayGet b 0) `shouldBe` Nothing
    (arrayGet b 1) `shouldBe` Nothing
    (arrayGet b (-1)) `shouldBe` Nothing

  it "should decode valid key-value pairs" $ do
    [r|{"key": "value"}|] `shouldBeKeyValuePairs` [("key", "value")]
    [r|{"key": 1}|] `shouldBeKeyValuePairs` [("key", "1")]
    [r|{"key": 9.999}|] `shouldBeKeyValuePairs` [("key", "9.999")]
    [r|{"key": true}|] `shouldBeKeyValuePairs` [("key", "true")]
    [r|{"key1": 10, "key2": "a", "key3": 1.1, "key4": false}|]
      `shouldBeKeyValuePairs`
      [ ("key1", "10")
      , ("key2", "a")
      , ("key3", "1.1")
      , ("key4", "false")
      ]

  it "should not decode invalid key-value pairs" $ do
    decodeKeyValuePair [r|{"key": null}|] `shouldSatisfy` isLeft
    decodeKeyValuePair [r|{"key": [1, "1"]}|] `shouldSatisfy` isLeft
    decodeKeyValuePair [r|{"key": {"nestedKey": true}}|] `shouldSatisfy` isLeft
    decodeKeyValuePair [r|{"key1": "a", "key2": {"nestedKey": true}}|] `shouldSatisfy` isLeft

  it "should decode request body" $ do
    decodePayload [r|{"bodyType": "urlencoded", "content": {"key1": "value1"}}|] `shouldSatisfy` isURLEncoded
    decodePayload [r|{"bodyType": "json", "content": {"key1": "value1"}}|] `shouldSatisfy` isJSON
    decodePayload [r|{"bodyType": "json", "content": ["key1", "value1"]}|] `shouldSatisfy` isJSON
    decodePayload [r|{"content": {"key1": "value1"}}|] `shouldSatisfy` isJSON
    decodePayload [r|{"other": {"key1": "value1"}}|] `shouldSatisfy` isJSON
    decodePayload [r|{"other": ["key1", "value1"]}|] `shouldSatisfy` isJSON
    -- "other" is ignored
    decodePayload [r|{"bodyType": "json", "content": {"a": "b"}, "other": 1}|] `shouldSatisfy` isJSON

  it "should not decode invalid request body" $ do
    decodePayload [r|{"bodyType": "plain", "content": {"key1": "value1"}}|] `shouldSatisfy` isLeft
    decodePayload [r|{"bodyType": "urlencoded"}|] `shouldSatisfy` isLeft
    decodePayload [r|{"bodyType": "json"}|] `shouldSatisfy` isLeft
    decodePayload [r|{"bodyType": "urlencoded", "content": ["key1", "value1"]}|] `shouldSatisfy` isLeft

testValidSpec :: String -> IO ()
testValidSpec file = do
  currentDirectory <- getCurrentDirectory
  spec <- decodeFile (currentDirectory ++ file)
  spec `shouldSatisfy` isRight

decodeKeyValuePair :: ByteString -> Either String KeyValuePairs
decodeKeyValuePair = eitherDecodeStrict

decodePayload :: ByteString -> Either String Payload
decodePayload = eitherDecodeStrict

shouldBeHeaders :: (Eq a, Show a) => Either a Headers -> [(T.Text, T.Text)] -> Expectation
shouldBeHeaders actual expected =
  let expectedHeaders = Right . HeaderSet $ uncurry Header <$> expected in
    actual `shouldBe` expectedHeaders

shouldBeKeyValuePairs :: ByteString -> [(A.KeyType, T.Text)] -> Expectation
shouldBeKeyValuePairs j expected = actual `shouldBe` expectedKeyValuePairs where
  actual = eitherDecodeStrict j
  expectedKeyValuePairs = Right . KeyValuePairs $ uncurry KeyValuePair <$> expected

isURLEncoded :: Either String Payload -> Bool
isURLEncoded (Right (URLEncoded _)) = True
isURLEncoded _                      = False

isJSON :: Either String Payload -> Bool
isJSON (Right (JSON _)) = True
isJSON _                = False
