{-|
Module      : Control.Concurrent.Async.Scattered
Description : Auto-linked Async Threads
Copyright   : (c) David Wilson, 2022
License     : BSD3

This module allows you to run `Async` threads
in a way that is in-between using `withAsync`
and just using `async`. Like `withAsync`, you
have the guarantee of having threads automatically
cancelled once execution leaves a `runThreads`
block, but like `async` you can spawn a new
thread anywhere within that `runThreads` block.
This is especially useful when the code in the
`runThreads` block is run with `forever`.
-}

module Control.Concurrent.Async.Scattered (
  -- * Basic Type(s)
  --
  -- | This is the type that handles linking threads
  -- and counting how many threads are currently 
  -- running. You access this through `runThreads`.
  ThreadManager,
  -- * Starting a ThreadManager
  --
  -- | These functions are what you use to start a
  -- `ThreadManager`. The standard way to use them
  -- is to do something like
  -- 
  -- > runThreads $ \mgr -> do
  -- >   asy1 <- startThread mgr action1
  -- >   asy2 <- startThread mgr action2
  -- >   asy3 <- startThread mgr action3
  -- >   rst1 <- wait asy1
  -- >   rst2 <- wait asy2
  -- >   rst3 <- wait asy3
  -- >   func rst1 rst2 rst3
  -- 
  -- Note that this example is more to show usage
  -- rather than usefulness; the above example could
  -- just as easily be done with multiple uses of
  -- `withAsync`. A more applicable use case would
  -- involve `forever`, and require threads to stay
  -- accessible (e.g. via some kind of map) from
  -- one run of the loop to the next.
  --
  -- __WARNING__ : If you return the `ThreadManager`
  -- from a `runThreads` block, and then try to spawn
  -- a new thread with it, in all likelihood that 
  -- thread will not automatically be cancelled. You
  -- can, however, use `getThreadCount` to read the
  -- number of threads still running. If you want to
  -- return a value that can't be used improperly,
  -- look at `ThreadCounter`.
  runThreads,
  runThreads',
  -- * Spawning Threads
  -- 
  -- | Spawning a new thread must be done within a
  -- `runThreads` or `runThreads'` block, and must
  -- use the `ThreadManager` associated with that
  -- block. 
  --
  -- The naming scheme for the various `startThread`
  -- functions is as follows:
  --
  --    - @S@ means that there is a "setup" aciton
  --      that occurs before starting the main thread;
  --      like the "before" part of a call to `bracket`.
  --      Note that for functions with an @S@, the other
  --      `IO` arguments ("action" and "closer") tkae an
  --      argument, which is supplied using the return
  --      value of the "setup" action.
  --      Also note that @S@ on its own implies @X@ (see next).
  --    - @X@ means that there is a closer on the function,
  --      similar to the "after" part of a call to `bracket`.
  --    - @E@ is similar to @X@, but the closer is only run
  --      when the thread receives an exception, making it
  --      similar to the "after" part of a call to
  --      `bracketOnError`.
  --    - @C@ works very similarly to @X@, but different
  --      functions are run depending on whether the 
  --      thread exited successfully, or if it received
  --      an exception.
  --
  -- Note that for all the various `startThread` functions,
  -- the "setup" and "closer" parts are run /after/ the linking
  -- and incrementing/decrementing of the thread count. This is
  -- because the "setups" and "closers" are user-supplied, and
  -- thus could block, making them cancellable before linking
  -- or incrementation/decrementation could occur.
  startThread,
  startThreadX,
  startThreadE,
  startThreadC,
  startThreadS,
  startThreadSE,
  startThreadSC,
) where

import Control.Concurrent.Async
import Control.Concurrent.Async.Scattered.Types
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever)