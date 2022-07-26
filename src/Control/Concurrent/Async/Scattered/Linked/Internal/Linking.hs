module Control.Concurrent.Async.Scattered.Linked.Internal.Linking (
  linkWrap,
) where

import Control.Concurrent.Async
import Control.Concurrent (myThreadId, ThreadId, forkIO, throwTo)
import Control.Monad (void)
import Control.Exception (mask, SomeException, try, Exception)
import Data.Kind (Type)

-- Link to a handler thread.
linkWrap :: forall (e :: Type) (a :: Type). (Exception e) => (SomeException -> Bool) -> (SomeException -> e) -> Async a -> IO ()
linkWrap shouldThrow wrapper a = do
  me <- myThreadId
  void $ forkRepeat $ do
    r <- waitCatch a
    case r of
      Left e | shouldThrow e -> throwTo me (wrapper e)
      _otherwise -> return ()

-- | Fork a thread that runs the supplied action, and if it raises an
-- exception, re-runs the action.  The thread terminates only when the
-- action runs to completion without raising an exception.
-- (Taken from Source for `Control.Concurrent.Async`.)
forkRepeat :: IO a -> IO ThreadId
forkRepeat action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkIO go

tryAll :: IO a -> IO (Either SomeException a)
tryAll = try
