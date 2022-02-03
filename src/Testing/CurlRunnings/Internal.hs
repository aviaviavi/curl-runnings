{-# LANGUAGE OverloadedStrings #-}

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
  , pShow
  , nowMillis
  , millisToS
  , LogLevel(..)
  , CurlRunningsLogger
  , CurlRunningsUnsafeLogger
  ) where

import           Control.Monad
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as TL
import           Debug.Trace
import           System.Clock
import qualified Text.Pretty.Simple as P

makeGreen :: T.Text -> T.Text
makeGreen s = "\x1B[32m" <> s <> "\x1B[0m"

makeRed :: T.Text -> T.Text
makeRed s = "\x1B[31m" <> s <> "\x1B[0m"

pShow :: Show a => a -> T.Text
pShow = TL.toStrict . P.pShow

tracer :: Show a => T.Text -> a -> a
tracer a b = trace (T.unpack $ a <> T.pack ": " <> pShow b) b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right v) = Right $ f v
mapRight _ (Left v)  = Left v

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left v)  = Left $ f v
mapLeft _ (Right v) = Right v

-- | Array indexing with negative values allowed
arrayGet :: [a] -> Int -> Maybe a
arrayGet a i
  | (i >= 0 && length a <= abs i) || null a || (i < 0 && length a <= (abs i - 1)) = Nothing
  | i >= 0 = Just $ a !! i
  | otherwise = Just $ a !! (length a + i)

data LogLevel
  = ERROR
  | INFO
  | DEBUG
  deriving (Show, Eq, Ord, Enum)

-- | A logger that respects the verbosity level given by input args
type CurlRunningsLogger = (LogLevel -> T.Text -> IO ())

-- | A tracer that respects the verbosity level given by input args. Logging
-- with this calls out to Debug.trace and can be used in pure code, but be aware
-- of the unsafe IO.
type CurlRunningsUnsafeLogger a = (LogLevel -> T.Text -> a -> a)

makeLogger :: LogLevel -> CurlRunningsLogger
makeLogger threshold level text = when (level <= threshold) $ putStrLn $ T.unpack text

makeUnsafeLogger :: Show a => LogLevel -> CurlRunningsUnsafeLogger a
makeUnsafeLogger threshold level text object =
  if level <= threshold
    then tracer text object
    else object

nowMillis :: IO Integer
nowMillis = do
  t <- getTime Realtime
  return $ (toNanoSecs t) `div` 1000000

millisToS :: Integer -> Double
millisToS t = (fromIntegral t :: Double) / 1000.0
