module Main where
import           System.Directory
import           Test.Hspec
import           Testing.CurlRunnings

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

main :: IO ()
main = hspec $ do
  describe "Curl Runnings" $ do
    it "should provide valid example yaml specs" $ do
      testValidSpec "/examples/example-spec.yaml"

    it "should provide valid example json specs" $ do
      testValidSpec "/examples/example-spec.json"

testValidSpec :: String -> IO ()
testValidSpec file = do
  currentDirectory <- getCurrentDirectory
  spec <- decodeFile (currentDirectory ++ file)
  spec `shouldSatisfy` isRight


