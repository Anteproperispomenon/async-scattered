# async-scattered

This is a simple package based on `async` that allows you to use a function like `withAsync` called `runThreads`, but instead of a single child thread that is tied to the calling thread, the calling thread can arbitrarily start new threads in the main action of `runThreads`, but are automatically cancelled when the main action of `runThreads` ends.

## Usage

Both `Linked` and `Pooled` versions have the same interface, but have different underlying implementations. At the moment, the `Pooled` version seems to be more memory-efficient.

### Example

Note: This example is meant to show usage, not purpose. This package is generally used more for when threads are arbitrarily created and cancelled throught the running of the main thread.

```haskell
main :: IO ()
main = do
  tq1 <- newTQueueIO
  tq2 <- newTQueueIO
  src <- ...
  runThreads $ \thdMgr -> do
    asy1 <- startThread thdMgr (stdinThread tq1)
    asy2 <- startThread thdMgr (otherInThread src tq1)
    asy3 <- startThread thdMgr (modifyThread tq1 tq2 reverse)
    withFile "output.txt" AppendMode $ \hnd -> forever $ do
      str <- atomically $ readTQueue tq2
      hPutStrLn hnd str

-- | Take lines from the user and write them to a queue
stdinThread :: TQueue String -> IO ()
stdinThread tq = forever $ do
  str <- getLine
  atomically $ writeTQueue tq str

-- | A different source of strings for the queue.
otherInThread :: ... -> TQueue String -> IO ()
otherInThread ... tq = forever $ do
  str <- ...
  atomically $ writeTQueue tq str

-- | Take strings from one queue, modify them somehow, and
-- write them to a second queue.
modifyThread :: TQueue String -> TQueue String -> (String -> String) -> IO ()
modifyThread tqIn tqOut f = forever $ atomically $ do
  str <- readTQueue tqIn
  writeTQueue tqOut (f str)
```

## Note

This package is in a very early state, and may not be ready for usage. It is more for my own usage and learning.
