import Suites.Async.Scattered.Simple

main :: IO ()
main = do -- putStrLn "Test suite not yet implemented"
  (thd1,thd2,thd3) <- simpleTest
  putStrLn $ "Initial Thread Count : " ++ show thd1
  putStrLn $ "Medial  Thread Count : " ++ show thd2
  putStrLn $ "Final   Thread Count : " ++ show thd3

