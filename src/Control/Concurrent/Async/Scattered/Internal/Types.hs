module Control.Concurrent.Async.Scattered.Internal.Types (
  ThreadManager(..),
  threadManagerId,
  dummyThread,
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
