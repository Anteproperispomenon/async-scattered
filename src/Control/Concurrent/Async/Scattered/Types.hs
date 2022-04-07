module Control.Concurrent.Async.Scattered.Types (
  -- * Types
  ThreadManager,
  -- * Starting the ThreadManager
  runThreads,
  runThreads',
  -- * Starting a Thread
  startThread,
  startThreadE,
  startThreadC,
  startThreadS,
  startThreadSE,
  startThreadSC,
  -- * Querying the ThreadManager
  getThreadCount,
  getThreadCountSTM,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_, bracketOnError)
import Data.Kind (Type)
import Control.Exception.Bracket (bracketChoice)

-- | The thread manager, which handles linking
-- of threads and counting running threads.
data ThreadManager = ThreadManager
  { tmDummy :: Async ()
  , tmCount :: TVar Integer
  } deriving (Eq)

-- The thread used to link other threads.
-- This isn't exported; it's use is hidden
-- from the enduser.
dummyThread :: IO ()
dummyThread = do
  tv <- newTVarIO False
  atomically $ do
    b <- readTVar tv
    check b

-- Just a simple function to run when starting a thread.
incTC :: ThreadManager -> IO ()
incTC tm = atomically $ modifyTVar' (tmCount tm) (+1)

-- Just a simple function to run when ending a thread.
decTC :: ThreadManager -> IO ()
decTC tm = atomically $ modifyTVar' (tmCount tm) (subtract 1)

-- | Run threads together with a `ThreadManager`
-- that handles creation of new threads.
runThreads :: forall (b :: Type). (ThreadManager -> IO b) -> IO b
runThreads actions = withAsync dummyThread $ \asy -> bracket
  (ThreadManager asy <$> newTVarIO 0)
  (\_ -> return ()) -- maybe more?
  actions  

-- | Run threads together with a `ThreadManager`
-- that handles creation of new threads. This version
-- allows you to examine the `ThreadManager` at the
-- end of execution, allowing you to check e.g.
-- how many threads are running.
runThreads' :: forall (b :: Type) (c :: Type). (ThreadManager -> IO c) -> (ThreadManager -> IO b) -> IO b
runThreads' closer actions = withAsync dummyThread $ \asy -> bracket
  (ThreadManager asy <$> newTVarIO 0)
  closer -- maybe more?
  actions

-- | Start a thread together with an action
-- to perform when the thread finishes. Note
-- that the closing action occurs before the
-- thread count is decremented.
startThread :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThread tm closer action = async $
  bracket_
    (do { linkOnly (const True) (tmDummy tm) ; incTC tm })
    (do { closer ; decTC tm })
    action

-- | Like `startThread`, but the closer is only used in errors.
startThreadE :: 
  forall (b :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO c -> IO (Async c)
startThreadE tm closer action = async $
  bracketOnError
    (do { linkOnly (const True) (tmDummy tm) ; incTC tm })
    (\_ -> do { closer ; decTC tm })
    (\_ -> action)

-- | Like `startThread`, but with separate closers
-- for success and exceptions.
startThreadC :: 
  forall (b :: Type) (b' :: Type) (c :: Type). 
  ThreadManager -> IO b -> IO b' -> IO c -> IO (Async c)
startThreadC tm closer closerErr action = async $
  bracketChoice
    (do { linkOnly (const True) (tmDummy tm) ; incTC tm })
    (\_ -> do { closer    ; decTC tm })
    (\_ -> do { closerErr ; decTC tm })
    (\_ -> action)

-- | Very similar to `bracket`, but with 
-- automatic linking. Note that the setup
-- occurs __after__ incrementing the thread
-- counter.
startThreadS ::
  forall (a :: Type) (b :: Type) (c :: Type).
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadS tm setup closer action = async $
  bracket
    (do { linkOnly (const True) (tmDummy tm) ; incTC tm ; setup })
    (\x -> do { z <- closer x ; decTC tm ; return z })    
    action

-- | Like `startThreadE`, but with a specific
-- thread setup action.
startThreadSE ::
  forall (a :: Type) (b :: Type) (c :: Type).
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO c) -> IO (Async c)
startThreadSE tm setup closer action = async $
  bracketOnError
    (do { linkOnly (const True) (tmDummy tm) ; incTC tm ; setup })
    (\x -> do { z <- closer x ; decTC tm ; return z })    
    action


-- | Very similar to `bracketChoice`, but with 
-- automatic linking.
startThreadSC ::
  forall (a :: Type) (b :: Type) (b' :: Type) (c :: Type).
  ThreadManager -> IO a -> (a -> IO b) -> (a -> IO b') -> (a -> IO c) -> IO (Async c)
startThreadSC tm setup closer closerErr action = async $
  bracketChoice
    (do { linkOnly (const True) (tmDummy tm) ; incTC tm ; setup })
    (\x -> do { z <- closer    x ; decTC tm ; return z })
    (\x -> do { z <- closerErr x ; decTC tm ; return z })
    action

-- | Get the current number of running threads
-- that are handled by this `ThreadManager`.
-- Note that this is fater than (`atomically 
-- `.` `getThreadCountSTM`), since it uses
-- `readTVarIO` in the underlying code.
getThreadCount :: ThreadManager -> IO Integer
getThreadCount (ThreadManager _ tc) = readTVarIO tc

-- | Get the current number of running threads
-- handled by the `ThreadManager`. Only use this
-- if you want to use it in an `STM` transaction,
-- since `getThreadCount` is faster than
-- (`atomically` `.` `getThreadCountSTM).
getThreadCountSTM :: ThreadManager -> STM Integer
getThreadCountSTM (ThreadManager _ tc) = readTVar tc

-- TODO: Write an exception type for when
-- a thread is cancelled by the thread 
-- manager. This makes it clearer to the
-- user what kind of cancellation is 
-- occurring and doesn't expose as much
-- of the inner workings of the package.
-- e.g. catch @(ExceptionInLinkedThread x).
