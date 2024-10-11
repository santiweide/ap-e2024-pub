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
          ref <- newIORef False
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobPending -- currently no workers so it should be pending now
          _ <- workerAdd spc "Spiderman"
          _ <- threadDelay 200 -- not blocking and I did not implement the jobWait...
          r2 <- jobStatus spc j
          r2 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
          v <- readIORef ref
          v @?= True
          j2 <- jobAdd spc $ Job (writeIORef ref False) 1
          _ <- threadDelay 200
          r3 <- jobStatus spc j2
          r3 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
          v2 <- readIORef ref
          v2 @?= False
          j3 <- jobAdd spc $ Job (writeIORef ref True) 1
          _ <- threadDelay 200
          r4 <- jobStatus spc j3
          r4 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
          v3 <- readIORef ref
          v3 @?= True
        , testCase "multi-worker-flow" $ do
          spc <- startSPC
          ref <- newIORef False
          j1 <- jobAdd spc $ Job (threadDelay 1000) 1 -- 1ms == 1000us
          j2 <- jobAdd spc $ Job (threadDelay 1000) 1
          j3 <- jobAdd spc $ Job (threadDelay 1000) 1
          r1 <- jobStatus spc j1
          r2 <- jobStatus spc j2
          r3 <- jobStatus spc j3
          r1 @?= JobPending
          r2 @?= JobPending 
          r3 @?= JobPending 
          _ <- workerAdd spc "Spiderwoman"
          _ <- workerAdd spc "Batwoman"
          _ <- workerAdd spc "Superwoman"
          r1 <- jobStatus spc j1
          r2 <- jobStatus spc j2
          r3 <- jobStatus spc j3
          r1 @?= JobRunning
          r2 @?= JobRunning
          r3 @?= JobRunning
          _ <- threadDelay 2000 -- keep enough time
          r1 <- jobStatus spc j1
          r2 <- jobStatus spc j2
          r3 <- jobStatus spc j3
          r1 @?= JobDone (DoneByWorker "Superwoman") 
          r2 @?= JobDone (DoneByWorker "Batwoman")
          r3 @?= JobDone (DoneByWorker "Spiderwoman")   


        -- Commented because No instance for (Eq (Server WorkerMsg))...long chain to add deriving Eq,Show
        -- import Control.Concurrent (Chan) the Chan does not support Show
        -- , testCase "worker-same-name" $ do
        --   spc <- startSPC
        --   ref <- newIORef False
        --   r1 <- workerAdd spc "Spiderman"
        --   r2 <- workerAdd spc "Spiderman"
        --   r2 @?= Left "WorkerName already exists"
      ]
