module Control.Concurrent.Async.Scattered.Pooled.Types (
    
  -- * Thread Manager
  ThreadManager,
  runThreads,
  cancelAll,
  -- * Starting Threads
  startThread,
  startThreadX,
  startThreadE,
  startThreadC,
  startThreadS,
  startThreadSE,
  startThreadSC,
  -- * Querying the Thread Manager
  getThreadCount,
  getThreadCountSTM,
) where

import Control.Concurrent.Async
import Control.Concurrent.Async.Scattered.Pooled.Internal.Types
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (void)
import Data.Kind (Type)
import ListT qualified as LT
import StmContainers.Set qualified as Set

-- | Start a new thread, using the 
-- `ThreadManager` to handle it.
-- This version includes an extra "closing"
-- action that is performed after the thread
-- has been removed from the pool.
startThreadX :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThreadX mgr@(ThreadManager thdSet) closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (do { atomically $ Set.delete this' thdSet ; closer })
      atomically $ Set.delete this' thdSet
      closer
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. Like
-- `startThreadX`, this function also
-- includes a closing action, but it
-- only runs when the thread exits on
-- exception.
startThreadE :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThreadE mgr@(ThreadManager thdSet) closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (do { atomically $ Set.delete this' thdSet ; closer })
      atomically $ Set.delete this' thdSet
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. This
-- function is essentially a combo of
-- `startThreadX` and `startThreadE`,
-- which runs different closing actions
-- depending on whether it exited
-- successfully or on exception.
startThreadC :: 
  forall (b :: Type) (b' :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO b' -> IO c -> IO (Async c)
startThreadC mgr@(ThreadManager thdSet) closer onErr action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      rslt <- unmask action `onException` (do { atomically $ Set.delete this' thdSet ; onErr })
      atomically $ Set.delete this' thdSet
      closer
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. This
-- version has a "setup" action that 
-- performs some work and returns a value
-- that is passed to both the main action,
-- as well as a closer action.
startThreadS :: 
  forall (a :: Type) (b :: Type) (c :: Type). 
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadS mgr@(ThreadManager thdSet) setup closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (action bolt) `onException` (do { atomically $ Set.delete this' thdSet ; closer bolt })
      atomically $ Set.delete this' thdSet
      closer bolt
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it.
-- Like `startThreadS`, but only runs
-- the closer action when the main
-- action exits on exception.
startThreadSE :: 
  forall (a :: Type) (b :: Type) (c :: Type). 
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadSE mgr@(ThreadManager thdSet) setup closer action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (action bolt) `onException` (do { atomically $ Set.delete this' thdSet ; closer bolt })
      atomically $ Set.delete this' thdSet
      return rslt
    putMVar mv asy
    return asy

-- | Start a new thread, using the 
-- `ThreadManager` to handle it. Like
-- `startThreadS`, but with different
-- closing actions to run depending on 
-- whether the main action exited
-- successfully or on exception.
startThreadSC :: 
  forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type). 
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async c)
startThreadSC mgr@(ThreadManager thdSet) setup closer onErr action =
  mask_ $ do
    mv <- newEmptyMVar
    asy <- asyncWithUnmask $ \unmask -> do
      this' <- AsyncX <$> takeMVar mv
      atomically $ Set.insert this' thdSet
      bolt <- setup
      rslt <- unmask (action bolt) `onException` (do { atomically $ Set.delete this' thdSet ; onErr bolt })
      atomically $ Set.delete this' thdSet
      closer bolt
      return rslt
    putMVar mv asy
    return asy

-- | Get the current number of running threads
-- that are handled by this `ThreadManager`.
-- Unlike the linked version, this is no
-- faster than the STM version.
getThreadCount :: ThreadManager -> IO Integer
getThreadCount (ThreadManager mgrSet) =
  fromIntegral <$> (atomically $ Set.size mgrSet)

-- | Get the current number of running threads
-- handled by the `ThreadManager`.
getThreadCountSTM :: ThreadManager -> STM Integer
getThreadCountSTM (ThreadManager mgrSet) =
  fromIntegral <$> Set.size mgrSet
