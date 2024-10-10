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
          r2 <- workerAdd spc "Spiderman"
          r1 @?= JobDone (DoneByWorker "Spiderman")
          v <- readIORef ref
          v @?= True
        -- Commented because No instance for (Eq (Server WorkerMsg))...long chain to add deriving Eq 
        -- , testCase "worker-same-name" $ do
        --   spc <- startSPC
        --   ref <- newIORef False
        --   r1 <- workerAdd spc "Spiderman"
        --   r2 <- workerAdd spc "Spiderman"
        --   r2 @?= Left "WorkerName already exists"
      ]
