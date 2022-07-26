import Suites.Async.Scattered.Pooled
import Suites.Async.Scattered.Simple

main :: IO ()
main = do -- putStrLn "Test suite not yet implemented"
  putStrLn "Linked Version Test"
  (thd1,thd2,thd3) <- simpleTest
  putStrLn $ "Initial Thread Count : " ++ show thd1
  putStrLn $ "Medial  Thread Count : " ++ show thd2
  putStrLn $ "Final   Thread Count : " ++ show thd3
  -- Pooled Test
  putStrLn "Pooled Version Test"
  (td1,td2,td3) <- pooledTest
  putStrLn $ "Initial Thread Count : " ++ show td1
  putStrLn $ "Medial  Thread Count : " ++ show td2
  putStrLn $ "Final   Thread Count : " ++ show td3
  

