-- | This module specifies any utilities used by this package. At this time,
-- consider everything in this module to be private to the curl-runnings package
module Testing.CurlRunnings.Internal
  ( makeRed
  , makeGreen
  , tracer
  , mapRight
  , mapLeft
  , arrayGet
  , makeLogger
  , makeUnsafeLogger

  , LogLevel(..)
  , CurlRunningsLogger
  , CurlRunningsUnsafeLogger
  ) where

import           Control.Monad
import           Debug.Trace

makeGreen :: String -> String
makeGreen s = "\x1B[32m" ++ s ++ "\x1B[0m"

makeRed :: String -> String
makeRed s = "\x1B[31m" ++ s ++ "\x1B[0m"

tracer :: Show a => String -> a -> a
tracer a b = trace (a ++ ": " ++ show b) b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right v) = Right $ f v
mapRight _ (Left v)  = Left v

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left v) = Left $ f v
mapLeft _ (Right v)  = Right v

-- | Array indexing with negative values allowed
arrayGet :: [a] -> Int -> a
arrayGet a i
  | i >= 0 = a !! i
  | otherwise = reverse a !! (-i)

data LogLevel = ERROR | INFO | DEBUG deriving (Show, Eq, Ord, Enum)

-- | A logger that respects the verbosity level given by input args
type CurlRunningsLogger = (LogLevel -> String -> IO ())

-- | A tracer that respects the verbosity level given by input args. Logging
-- with this calls out to Debug.trace and can be used in pure code, but be aware
-- of the unsafe IO.
type CurlRunningsUnsafeLogger a = (LogLevel -> String -> a -> a)

makeLogger :: LogLevel -> CurlRunningsLogger
makeLogger threshold level text =
  when (level <= threshold) $ putStrLn text

makeUnsafeLogger :: Show a => LogLevel -> CurlRunningsUnsafeLogger a
makeUnsafeLogger threshold level text object =
  if level <= threshold then
    tracer text object
  else
    object


