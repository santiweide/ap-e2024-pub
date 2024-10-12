module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [
        testCase "job-work-flow" $ do
          spc <- startSPC
          ref <- newIORef (1 :: Int)
          j <- jobAdd spc $ Job (writeIORef ref 1) 1
          r1 <- jobStatus spc j
          r1 @?= JobPending -- currently no workers so it should be pending now
          _ <- workerAdd spc "Neko"
          _ <- threadDelay 200 -- not blocking and I did not implement the jobWait...
          r2 <- jobStatus spc j
          r2 @?= JobDone (DoneByWorker "Neko") -- add a worker so the job status would be Done
          v <- readIORef ref
          v @?= 1
          j2 <- jobAdd spc $ Job (writeIORef ref 2) 1
          _ <- threadDelay 200
          r3 <- jobStatus spc j2
          r3 @?= JobDone (DoneByWorker "Neko") -- add a worker so the job status would be Done
          v2 <- readIORef ref
          v2 @?= 2
          j3 <- jobAdd spc $ Job (writeIORef ref 3) 1
          _ <- threadDelay 200
          r4 <- jobStatus spc j3
          r4 @?= JobDone (DoneByWorker "Neko") -- add a worker so the job status would be Done
          v3 <- readIORef ref
          v3 @?= 3
        , testCase "job-wait" $ do -- dont like sync way here but still implement it
          spc <- startSPC
          ref <- newIORef (1 :: Int)
          j1 <- jobAdd spc $ Job (writeIORef ref 1) 1
          _ <- workerAdd spc "Mickey"
          r1 <- jobWait spc j1
          r1 @?= (DoneByWorker "Mickey") 
          v <- readIORef ref
          v @?= 1
          j2 <- jobAdd spc $ Job (writeIORef ref 2) 1
          r2 <- jobWait spc j2
          r2 @?= (DoneByWorker "Mickey")
          v2 <- readIORef ref
          v2 @?= 2
          j3 <- jobAdd spc $ Job (writeIORef ref 3) 1
          r3 <- jobWait spc j3
          r3 @?= (DoneByWorker "Mickey")
          v3 <- readIORef ref
          v3 @?= 3
        , testCase "multi-worker-flow" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (threadDelay 100) 3 -- 1ms == 1000us
          j2 <- jobAdd spc $ Job (threadDelay 100) 3
          j3 <- jobAdd spc $ Job (threadDelay 100) 3
          r1 <- jobStatus spc j1
          r2 <- jobStatus spc j2
          r3 <- jobStatus spc j3
          r1 @?= JobPending
          r2 @?= JobPending 
          r3 @?= JobPending 
          _ <- workerAdd spc "Spiderwoman"
          _ <- workerAdd spc "Batwoman"
          _ <- workerAdd spc "Catwoman"
          r4 <- jobStatus spc j1
          r5 <- jobStatus spc j2
          r6 <- jobStatus spc j3
          r4 @?= JobRunning
          r5 @?= JobRunning
          r6 @?= JobRunning
          _ <- threadDelay 200 -- keep enough time
          r7 <- jobStatus spc j1
          r8 <- jobStatus spc j2
          r9 <- jobStatus spc j3
  -- TODO order when the threadDelay is the same.. or when the delay is different it will be okay
          r7 @?= JobDone (DoneByWorker "Catwoman")
          r8 @?= JobDone (DoneByWorker "Batwoman")
          r9 @?= JobDone (DoneByWorker "Spiderwoman")   
        , testCase "job-cancel" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (threadDelay 1000) 1 -- 1ms == 1000us
          j2 <- jobAdd spc $ Job (threadDelay 1000) 1
          j3 <- jobAdd spc $ Job (threadDelay 1000) 1
          _ <- workerAdd spc "Peter"
          _ <- threadDelay 20
          r1 <- jobStatus spc j1
          r2 <- jobStatus spc j2
          r3 <- jobStatus spc j3
          r1 @?= JobPending
          r2 @?= JobPending
          r3 @?= JobRunning
          _ <- jobCancel spc j3
          _ <- threadDelay 20000 -- wait for everything done
          r4 <- jobStatus spc j1
          r5 <- jobStatus spc j2
          r6 <- jobStatus spc j3
          r4 @?= JobDone (DoneByWorker "Peter")
          r5 @?= JobDone (DoneByWorker "Peter")
          r6 @?= JobDone DoneCancelled
        , testCase "job-timeout-centralized-0" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (threadDelay 2000) 0 -- fast failed
          _ <- workerAdd spc "RogBrod"
          _ <- threadDelay 100
          r1 <- jobStatus spc j1
          r1 @?= JobDone (DoneTimeout)
          j2 <- jobAdd spc $ Job (threadDelay 2000) 1
          _ <- threadDelay 20
          r2 <- jobStatus spc j2 -- test whether Momo could done other jobs
          r2 @?= JobRunning -- TODO test shows it is pending so what
          _ <- threadDelay 3000 -- A thousand years later
          r3 <- jobStatus spc j2
          r3 @?= JobDone (DoneByWorker "RogBrod")
        , testCase "job-timeout-centralized-1" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (threadDelay 1001000) 1 -- fast failed
          _ <- workerAdd spc "Jacob"
          r1 <- jobWait spc j1 -- just wait bcz 1000000 delay is not enough
          r1 @?= DoneTimeout
          j2 <- jobAdd spc $ Job (threadDelay 2000) 1
          _ <- threadDelay 20
          r2 <- jobStatus spc j2 -- test whether Momo could done other jobs
          r2 @?= JobRunning -- TODO test shows it is pending so what
          _ <- threadDelay 3000 -- A thousand years later
          r3 <- jobStatus spc j2
          r3 @?= JobDone (DoneByWorker "Jacob")
        -- Commented because No instance for (Eq (Server WorkerMsg))...long chain to add deriving Eq,Show
        -- import Control.Concurrent (Chan) the Chan does not support Show
        -- TODO how to test this..
        -- , testCase "worker-same-name" $ do
        --   spc <- startSPC
        --   r1 <- workerAdd spc "Spiderman"
        --   r2 <- workerAdd spc "Spiderman"
          
      ]
