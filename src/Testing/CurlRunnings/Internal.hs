-- | This module specifies any utilities used by this package. At this time,
-- consider everything in this module to be private to the curl-runnings package
module Testing.CurlRunnings.Internal (
    makeRed
  , makeGreen
                                     ) where

makeGreen :: String -> String
makeGreen s = "\x1B[32m" ++ s ++ "\x1B[0m"

makeRed :: String -> String
makeRed s = "\x1B[31m" ++ s ++ "\x1B[0m"
