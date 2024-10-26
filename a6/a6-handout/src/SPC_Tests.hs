module SPC_Tests (tests) where
-- TODO replicate
import Control.Concurrent (threadDelay)
-- TODO how to test a list of jobs in a monadic style? 
-- import Control.Monad (replicateM) 
-- import Control.Monad (forM, forM_, replicateM) 
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
-- import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup "SPC (core)" $
        concat $ [ -- functionality test
          -- replicate 5 jobWorkFlowTestCase
          map jobWorkFlowTestCase [1..2] 
          , replicate 2 jobTimeout1TestCase 
          , replicate 2 jobMultiWorkFlowTestCase
        ] ++
        [ -- concurrency test
          replicate 100 jobWaitWorkTestCase
          , replicate 100 jobCanceledTestCase 
          , replicate 100 jobPendingCanceledTestCase
          , replicate 100 jobCrashedTestCase
          , replicate 100 workerRunningStopTestCase
          , replicate 100 workerNoJobStopTestCase
        ]

-- specific test cases
jobWorkFlowTestCase :: Int -> TestTree
jobWorkFlowTestCase num = 
  testCase "job-work-flow" $ do
    spc <- startSPC
    ref <- newIORef (num :: Int)
    j <- jobAdd spc $ Job (writeIORef ref $ num + 1) 1
    r1 <- jobStatus spc j
    r1 @?= JobPending -- currently no workers so it should be pending now
    _ <- workerAdd spc "Neko"
    _ <- threadDelay 5000 
    r2 <- jobStatus spc j
    r2 @?= JobDone Done -- add a worker so the job status would be Done
    v <- readIORef ref
    v @?= num + 1
    j2 <- jobAdd spc $ Job (writeIORef ref $ num + 2) 1
    _ <- threadDelay 100
    r3 <- jobStatus spc j2
    r3 @?= JobDone Done -- add a worker so the job status would be Done
    v2 <- readIORef ref
    v2 @?= num + 2
    j3 <- jobAdd spc $ Job (writeIORef ref $ num + 3) 1
    _ <- threadDelay 100
    r4 <- jobStatus spc j3
    r4 @?= JobDone Done -- add a worker so the job status would be Done
    v3 <- readIORef ref
    v3 @?= num + 3

jobWaitWorkTestCase :: TestTree
jobWaitWorkTestCase = 
  testCase "job-wait" $ do -- dont like sync way here but still implement it
    spc <- startSPC
    ref <- newIORef (1 :: Int)
    j1 <- jobAdd spc $ Job (writeIORef ref 1) 1
    _ <- workerAdd spc "Mickey"
    r1 <- jobWait spc j1
    r1 @?= Done
    v <- readIORef ref
    v @?= 1
    j2 <- jobAdd spc $ Job (writeIORef ref 2) 1
    r2 <- jobWait spc j2
    r2 @?= Done
    v2 <- readIORef ref
    v2 @?= 2
    j3 <- jobAdd spc $ Job (writeIORef ref 3) 1
    r3 <- jobWait spc j3
    r3 @?= Done
    v3 <- readIORef ref
    v3 @?= 3

jobMultiWorkFlowTestCase :: TestTree
jobMultiWorkFlowTestCase = 
  testCase "multi-worker-flow" $ do
    spc <- startSPC
    j1 <- jobAdd spc $ Job (threadDelay 2000) 3 -- 1ms == 1000us
    r1 <- jobStatus spc j1
    r1 @?= JobPending
    _ <- workerAdd spc "Catwoman"
    j2 <- jobAdd spc $ Job (threadDelay 2000) 3
    r2 <- jobStatus spc j2
    r2 @?= JobPending 
    _ <- workerAdd spc "Batwoman"
    j3 <- jobAdd spc $ Job (threadDelay 2000) 3
    r3 <- jobStatus spc j3
    r3 @?= JobPending 
    _ <- workerAdd spc "Spiderwoman"
    r4 <- jobWait spc j1
    r5 <- jobWait spc j2
    r6 <- jobWait spc j3
  -- TODO order when the threadDelay is the same.. or when the delay is different it will be okay
    r4 @?= Done
    r5 @?= Done
    r6 @?= Done


jobPendingCanceledTestCase :: TestTree
jobPendingCanceledTestCase = 
  testCase "job-pending-cancel" $ do
    spc <- startSPC
    j1 <- jobAdd spc $ Job (threadDelay 1) 1 -- 1ms == 1000us TOBE cancelled so have a loonger
    _ <- jobCancel spc j1
    r1 <- jobStatus spc j1
    r1 @?= JobPending

jobCanceledTestCase :: TestTree
jobCanceledTestCase = 
  testCase "job-cancel" $ do
    spc <- startSPC
    j1 <- jobAdd spc $ Job (threadDelay 1000000) 1 -- 1ms == 1000us TOBE cancelled so have a loonger
    _ <- workerAdd spc "Peter"
    _ <- threadDelay 100
    r1 <- jobStatus spc j1
    r1 @?= JobRunning
    j2 <- jobAdd spc $ Job (threadDelay 1000) 1
    j3 <- jobAdd spc $ Job (threadDelay 1000) 1
    r2 <- jobStatus spc j2
    r3 <- jobStatus spc j3
    r2 @?= JobPending
    r3 @?= JobPending
    _ <- jobCancel spc j1
    r4 <- jobWait spc j1
    r5 <- jobWait spc j2
    r6 <- jobWait spc j3
    r4 @?= DoneCancelled
    r5 @?= Done
    r6 @?= Done

jobTimeout1TestCase :: TestTree
jobTimeout1TestCase = 
  testCase "job-timeout-centralized-1" $ do
    spc <- startSPC
    j1 <- jobAdd spc $ Job (threadDelay 2000000) 1 -- fast failed
    _ <- threadDelay 1000000 -- let MsgTick happen
    _ <- workerAdd spc "Jacob"
    r1 <- jobWait spc j1 -- just wait bcz 1000000 delay is not enough
    r1 @?= DoneTimeout
    j2 <- jobAdd spc $ Job (threadDelay 2000) 1
    r2 <- jobWait spc j2
    r2 @?= Done

jobCrashedTestCase :: TestTree
jobCrashedTestCase = 
  testCase "job-crashed" $ do
    spc <- startSPC
    ref <- newIORef False
    j1 <- jobAdd spc $ Job (error "crash!") 1 
    r1 <- jobStatus spc j1
    r1 @?= JobPending
    _ <- workerAdd spc "Catlady"
    r2 <- jobWait spc j1 -- just wait bcz 1000000 delay is not enough
    r2 @?= DoneCrashed
    j3 <- jobAdd spc $ Job (writeIORef ref True) 1
    r3 <- jobWait spc j3
    r3 @?= Done
    v <- readIORef ref
    v @?= True

workerRunningStopTestCase :: TestTree
workerRunningStopTestCase = 
  testCase "worker-jobbing-stop" $ do
    spc <- startSPC
    ref <- newIORef (0 :: Int)
    j1 <- jobAdd spc $ Job (threadDelay 100000 >> writeIORef ref 1) 1 -- run for 100 ms
    w1 <- workerAdd spc "Starbucks"
    case w1 of 
      Left err -> assertFailure err
      Right worker -> do
        r1 <- jobStatus spc j1
        r1 @?= JobRunning --both the worker and job are occupied
        _ <- workerStop worker
        r2 <- jobWait spc j1
        r2 @?= DoneCancelled --both the worker and job are occupied
        j2 <- jobAdd spc $ Job (writeIORef ref 2) 1
        r3 <- jobStatus spc j2
        r3 @?= JobPending -- if this is running then the worker is not actually killed
        _ <- workerAdd spc "Manners"
        r4 <- jobWait spc j2
        r4 @?= Done
        v <- readIORef ref
        v @?= 2

workerNoJobStopTestCase :: TestTree
workerNoJobStopTestCase = 
  testCase "worker-no-job-stop" $ do
    spc <- startSPC
    ref <- newIORef (0 :: Int)
    j1 <- jobAdd spc $ Job (threadDelay 100 >> writeIORef ref 1) 1 -- run for 0.1ms
    w1 <- workerAdd spc "Noodles"
    case w1 of 
      Left err -> assertFailure err
      Right worker -> do
        r1 <- jobStatus spc j1
        r1 @?= JobRunning --both the worker and job are occupied
        r2 <- jobWait spc j1
        r2 @?= Done --both the worker and job are occupied
        v1 <- readIORef ref
        v1 @?= 1
        _ <- workerStop worker
        j2 <- jobAdd spc $ Job (writeIORef ref 2) 1
        r3 <- jobStatus spc j2
        r3 @?= JobPending -- if this is running then the worker is not actually killed
        _ <- workerAdd spc "Coffee"
        r4 <- jobWait spc j2
        r4 @?= Done
        v2 <- readIORef ref
        v2 @?= 2


-- workerSameName :: Int -> TestTree
-- workerSameName _ = 
-- -- Commented because No instance for (Eq (Server WorkerMsg))...long chain to add deriving Eq,Show
-- -- import Control.Concurrent (Chan) the Chan does not support Show TDp
-- -- TODO how to test this..
--     testCase "worker-same-name" $ do
--       spc <- startSPC
--       r1 <- workerAdd spc "Spiderman"
--       r2 <- workerAdd spc "Spiderman"
      