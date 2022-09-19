{-# LANGUAGE NumericUnderscores #-}

module Linked (
  simpleTest,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Async.Scattered.Linked.Types
import Control.Concurrent.STM
import Control.Monad
import Data.List (cycle)

simpleTest :: IO (Integer, Integer, Integer)
simpleTest = do
  -- To prevent threads from finishing early.
  gate <- newTVarIO False
  rslt <- runThreads $ \manager -> do
    -- Create a list with varying delays.
    let lst = take 500 $ cycle [3..10]
    -- Spawn the threads.
    mapM_ (\n -> startThread manager (basicThread gate n)) lst
    -- Delay, for safety
    threadDelay 300_000
    -- Get the # of threads
    thrds <- getThreadCount manager
    -- Allow the threads to start running
    atomically $ writeTVar gate True
    -- Wait some time
    threadDelay 6_500_000
    -- Get the new thread count.
    thrds' <- getThreadCount manager
    -- Return some values
    return ((thrds,thrds'),manager)
  -- Extract results
  let thrds1 = fst $ fst rslt
      thrds2 = snd $ fst rslt
      mgr    = snd $ rslt
  threadDelay 5000
  thrds3 <- getThreadCount mgr
  return (thrds1, thrds2, thrds3)

basicThread :: TVar Bool -> Int -> IO ()
basicThread tv n = do
  -- Wait until gate is True before starting countdown.
  atomically (readTVar tv >>= check)
  threadDelay (n * 1_000_000)

