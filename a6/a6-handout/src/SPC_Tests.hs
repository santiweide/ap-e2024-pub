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
          r3 <- jobStatus spc j
          -- r3 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
          v <- readIORef ref
          v @?= True
          j2 <- jobAdd spc $ Job (writeIORef ref False) 1
          r4 <- jobStatus spc j2
          -- r4 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
          v2 <- readIORef ref
          v2 @?= False
          j3 <- jobAdd spc $ Job (writeIORef ref True) 1
          r5 <- jobStatus spc j2
          -- r4 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
          v3 <- readIORef ref
          v3 @?= True
        -- , testCase "job-work-flow-longer" $ do
        --   spc <- startSPC
        --   ref <- newIORef False
        --   j <- jobAdd spc $ Job (writeIORef ref True) 1
        --   r1 <- jobStatus spc j
        --   r1 @?= JobPending -- currently no workers so it should be pending now
        --   r2 <- workerAdd spc "Spiderman"
        --   j2 <- jobAdd spc $ Job (writeIORef ref False) 1
        --   r3 <- jobStatus spc j2
        --   r3 @?= JobDone (DoneByWorker "Spiderman") -- add a worker so the job status would be Done
        --   v <- readIORef ref
        --   v @?= False
        -- Commented because No instance for (Eq (Server WorkerMsg))...long chain to add deriving Eq,Show
        -- import Control.Concurrent (Chan) the Chan does not support Show
        -- , testCase "worker-same-name" $ do
        --   spc <- startSPC
        --   ref <- newIORef False
        --   r1 <- workerAdd spc "Spiderman"
        --   r2 <- workerAdd spc "Spiderman"
        --   r2 @?= Left "WorkerName already exists"
      ]
