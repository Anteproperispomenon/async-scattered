module Control.Concurrent.Async.Scattered.Internal.Exceptions (
  ThreadHandlerException(..),
  wrapHandlerException,
) where

import Control.Concurrent.Async
import Control.Exception

-- | Like `Control.Concurrent.Async.ExceptionInLinkedThread`,
-- but with more specific usage.
data ThreadHandlerException 
  = ThreadHandlerEnded (Async ())
  | ThreadHandlerException (Async ()) SomeException

-- Based on instance for ExceptionInLinkedThread.
instance Show ThreadHandlerException  where
  showsPrec p (ThreadHandlerEnded asy) =
    showParen (p >= 11) $
      showString "ThreadHandlerEnded " .
      showsPrec 11 t
    where t = asyncThreadId asy
  showsPrec p (ThreadHandlerException asy e) =
    showParen (p >= 11) $
      showString "ThreadHandlerException " .
      showsPrec 11 t .
      showString " " .
      showsPrec 11 e
    where t = asyncThreadId asy

instance Exception ThreadHandlerException

wrapHandlerException :: Async () -> SomeException -> ThreadHandlerException
wrapHandlerException asy e 
  | (Just _) <- fromException @AsyncCancelled e
  = ThreadHandlerEnded asy
  | otherwise = ThreadHandlerException asy e





