{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either
import           System.Directory
import           Test.Hspec
import           Testing.CurlRunnings
import           Testing.CurlRunnings.Internal
import           Testing.CurlRunnings.Internal.Parser

main :: IO ()
main = hspec $
  describe "Curl Runnings" $ do
  it "should provide valid example yaml specs" $
    testValidSpec "/examples/example-spec.yaml"

  it "should provide valid example json specs" $
    testValidSpec "/examples/example-spec.json"

  -- note that this doesn't actually try to parse the interpolations themselves,
  -- but that would be useful thing to add here
  it "should provid a valid interpolation spec" $
    testValidSpec "/examples/interpolation-spec.yaml"

  it "should parse valid queries" $ do
    parseQuery "just some text" `shouldSatisfy` isRight
    parseQuery "$<RESPONSES[0].key.key>" `shouldSatisfy` isRight
    parseQuery "$<RESPONSES[0].key.key[0].key_with_underscores>" `shouldSatisfy` isRight
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

  it "arrayGet should handle positive and negative indexes correctly" $ do
    let a = [1, 2, 3]
        b = [] :: [Int]
    (arrayGet a 0) `shouldBe` Just 1
    (arrayGet a 1) `shouldBe` Just 2
    (arrayGet a 2) `shouldBe` Just 3
    (arrayGet a (-1)) `shouldBe` Just 3
    (arrayGet a (-2)) `shouldBe` Just 2
    (arrayGet a (-3)) `shouldBe` Just 1
    (arrayGet a (-4)) `shouldBe` Nothing
    (arrayGet a 3) `shouldBe` Nothing
    (arrayGet b 0) `shouldBe` Nothing
    (arrayGet b 1) `shouldBe` Nothing
    (arrayGet b (-1)) `shouldBe` Nothing

testValidSpec :: String -> IO ()
testValidSpec file = do
  currentDirectory <- getCurrentDirectory
  spec <- decodeFile (currentDirectory ++ file)
  spec `shouldSatisfy` isRight


