module Control.Concurrent.Async.Scattered.Linked.Internal.Types (
  ThreadManager(..),
  ThreadCounter(..),
  threadManagerId,
  getThreadCounter,
  dummyThread,
  incTC,
  decTC,
) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Monad.STM (atomically, check)

-- | The thread manager, which handles linking
-- of threads and counting running threads.
data ThreadManager = ThreadManager
  { tmDummy :: Async ()
  , tmCount :: TVar Integer
  } deriving (Eq)

-- | Get a string representation of the `ThreadId`
-- of the `ThreadManager`.
threadManagerId :: ThreadManager -> String
threadManagerId (ThreadManager {tmDummy = tm}) = show $ asyncThreadId tm

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

-- | A variant of `ThreadManager` that just includes
-- the thread counting portion of it. This can safely
-- be returned from a `runThreads` block without the
-- possibility of spawning new threads. This can be
-- useful if you are writing a library, but still
-- want to expose the number of threads running to
-- the enduser.
newtype ThreadCounter = ThreadCounter
  { tcCount :: TVar Integer }
  deriving (Eq)

-- | Get a `ThreadCounter` that can be queried for
-- the number of threads tied to a specific
-- `ThreadManager` that are still running.
getThreadCounter :: ThreadManager -> ThreadCounter
getThreadCounter = ThreadCounter . tmCount


