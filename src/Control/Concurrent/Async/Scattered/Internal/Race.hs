module Control.Concurrent.Async.Scattered.Internal.Race (
  raceTime,
  runWithTimeout,
  ThreadTimeout(..),
) where

import Control.Concurrent.Async
import Control.Exception
import Control.Concurrent (threadDelay)

-- First thread is the "timer" thread.
raceTime :: forall a b. IO a -> IO b -> IO (Maybe b)
raceTime timer action = maybeEither <$> race timer action'
  where 
    action' :: IO b
    action' = catch @AsyncCancelled action (\_ -> throwIO ThreadTimeout)
    maybeEither :: forall x y. Either x y -> Maybe y
    maybeEither (Left  _) = Nothing
    maybeEither (Right x) = Just x

data ThreadTimeout 
  = ThreadTimeout
  deriving (Show, Eq)

instance Exception ThreadTimeout

-- | Run an action with a timeout value, in microseconds.
runWithTimeout :: forall a. Int -> IO a -> IO (Maybe a)
runWithTimeout n = raceTime (threadDelay n)
