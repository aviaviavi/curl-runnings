module Testing.CurlRunnings.Internal (
  makeRed
  , makeGreen
                                     ) where

makeGreen :: String -> String
makeGreen s = "\x1B[32m" ++ s ++ "\x1B[0m"

makeRed :: String -> String
makeRed s = "\x1B[31m" ++ s ++ "\x1B[0m"
