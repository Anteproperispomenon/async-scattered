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
  -- >   asy1 <- startThread mgr (return ()) action1
  -- >   asy2 <- startThread mgr (return ()) action2
  -- >   asy3 <- startThread mgr (return ()) action3
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
  -- (Put info about naming scheme here)
  startThread,
  startThreadE,
  startThreadC,
  startThreadS,
  startThreadSE,
  startThreadSC,
) where

import Control.Concurrent.Async.Scattered.Types