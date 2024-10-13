module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
    ThreadId
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forever, liftM, void, forM_)
import Data.List(partition, find)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

remove :: Eq a => a -> [a] -> [a]
remove x = filter (/= x)
-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }
instance Show Job where
  show job = "Job { jobMaxSeconds = " ++ show (jobMaxSeconds job) ++ " }"


-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  | DoneByWorker WorkerName
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg
 = -- | assign Job and JobId to the worker
   -- | partial sync, make sure to get thread id updated. But cannot make sure the job is done.
   -- | so its name is just assign.
  MsgAssignJob Job JobId (ReplyChan ())
 | -- | the worker will stop
  MsgStopAndBreakLoop (ReplyChan ())

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  | -- | check if the worker exists. 
    MsgWorkerExists WorkerName (ReplyChan Bool)
  | -- | inform a job is done by worker, recording worker name
    MsgJobDoneByWorker JobId WorkerName (ReplyChan ())
  | -- | add a worker
    MsgAddWorker  WorkerName Worker (ReplyChan SPCState)
  | -- | update the running job with  Async, risk
    -- |   worker name=@WorkerName that it is running thread id=@tid
    MsgUpdateRunningWithTid WorkerName ThreadId
  | -- | Job crashed.
    MsgJobCrashed JobId
  | -- | get worker name by job id. 
    -- |    precheck: worker is running, if not running then return Nothing
    MsgGetJobIdByWorkerName WorkerName  (ReplyChan (Maybe JobId))
  | -- | remove the worker from the state map
    MsgRemoveWorker WorkerName (ReplyChan ())
  | -- | do job cancel and worker remove syncly
    MsgRemoveWorkersJobIfAnyRunning WorkerName (ReplyChan ())


-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, (Seconds, Maybe ThreadId, WorkerName, Job))],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (JobDoneReason))], -- save handler for callback
    spcJobCounter :: JobId,
    spcWorkers :: [(WorkerName, Worker)], -- has Server inside
    spcWorkersIdle :: [WorkerName],
    spcChan :: Chan SPCMsg -- a handler for requesting access 
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

-- Only SPCM can modify the states, so we should gaurantee the SPCM orders are in sequence
-- schedule is acted one at a time to prevent the worker competition
schedule :: SPCM ()
schedule = do
  state <- get
  --- BEGIN DEBUG
  -- state' <- get
  -- io $ putStrLn $ unlines 
  --   [
  --     "DEBUG in SCHEDULE_BEGIN",
  --     "\tPending " ++ (show (spcJobsPending state')),
  --     "\tRunning " ++ (show (spcJobsRunning state')),
  --     "\tDone " ++ (show (spcJobsDone state'))
  --     -- "\tCounter " ++ (show (spcJobCounter state'))
  --   ]

  case (spcWorkersIdle state, spcJobsPending state) of
    (workerName : idleWorkers, (jobId, job) : pendingJobs) -> do
      case lookup workerName (spcWorkers state) of
        Just (Worker workerServer) -> do
          now <- io $ getSeconds
          -- Update the state with the new list of idle workers and running jobs
          let deadline = now + fromIntegral (jobMaxSeconds job)
              updatedRunningJobs = (jobId, (deadline, Nothing, workerName, job)) : spcJobsRunning state
          modify $ \s -> s
              { spcWorkersIdle = idleWorkers, -- leave the idle pool
                spcJobsPending = pendingJobs,
                spcJobsRunning = updatedRunningJobs
              }
          -- Send the job to the worker (non-blocking, asynchronous)
          -- state' <- get
          -- io $ putStrLn $ unlines 
          --   [
          --     "DEBUG in SCHEDULE_END",
          --     "\tjob added " ++ show jobId,
          --     "\tPending " ++ (show (spcJobsPending state')),
          --     "\tRunning " ++ (show (spcJobsRunning state')),
          --     "\tDone " ++ (show (spcJobsDone state'))
          --     -- "\tCounter " ++ (show (spcJobCounter state'))
          --   ]
          io $ requestReply workerServer (MsgAssignJob job jobId)
        Nothing -> pure ()  -- Handle the case where the worker wasn't found, should never reach here
    _ -> pure ()  -- No idle workers or no pending jobs, do nothing

-- only do to jobs' state
jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  state <- get
  case lookup jobId $ spcJobsDone state of
    Just _ -> do 
      pure () -- already done
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobId) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ reason -- late reply to jobWait
      modify $ \s -> s 
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobId, reason) : spcJobsDone state,
            spcJobsRunning = removeAssoc jobId $ spcJobsRunning state
          }
      -- state' <- get
      -- io $ putStrLn $ unlines 
      --        [ "INSIDE jobDONE:", 
      --         "job " ++ show jobId,
      --        "jobsDone:",
      --         show (spcJobsDone state'),
      --         "jobsRunning:",
      --         show (spcJobsRunning state') ]

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle = 
  modify $ \s -> s { spcWorkersIdle = workerName : spcWorkersIdle state }

workerIsGone :: WorkerName -> SPCM ()
workerIsGone =       
  modify $ \s -> 
    let 
        (runningJobsForWorker, otherRunningJobs) = partition (\(_, (_, _, wName, _)) -> wName == workerName) (spcJobsRunning s)
        jobsToMoveBack = map (\(jobId, (_, _, _, job)) -> (jobId, job)) runningJobsForWorker
    in s
      { spcWorkers = removeAssoc workerName $ spcWorkers s, 
        spcWorkersIdle = remove workerName $ spcWorkersIdle s, 
        spcJobsRunning = otherRunningJobs, 
        spcJobsPending = jobsToMoveBack ++ spcJobsPending s 
      }

-- guarantee no state change while doing checkTimeout, thread safe.
checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  -- io $ putStrLn $ "[checkTimeouts] current_time=" ++ show now
  let runningJobs = spcJobsRunning state
      (timedOutJobs, _) = 
          partition (\(_, (deadline, _, _, _)) -> now >= deadline) runningJobs

  forM_ timedOutJobs $ \(jobId, ( _, maybe_tid, workerName, _)) -> 
    case maybe_tid of
      Just tid -> do
        io $ killThread tid 
        jobDone jobId DoneTimeout -- chage jobs state -- TODO add a batch jobDone to reduce concurrency
        workerIsIdle
      Nothing -> do
        io $ threadDelay 10 -- TODO magic number
        checkTimeouts

workerExists :: WorkerName -> SPCM Bool
workerExists workerName = do
  state <- get
  pure $ any ((== workerName) . fst) (spcWorkers state)

-- only do IO no state change. only state change in SPC in sequence.
handleWorkerMsg :: Chan WorkerMsg -> SPC -> WorkerName -> IO ()
handleWorkerMsg c (SPC spc) workerName = do
  msg <- receive c -- blocking IO
  case msg of
    MsgAssignJob job jobId rsvp -> do
      tid <- forkIO $ do -- this thread id is not the same as the worker thread id...
        let doJob = do
              jobAction job
              requestReply spc $ MsgJobDoneByWorker jobId workerName 
            onException :: SomeException -> IO ()
            onException _ =
                sendTo spc $ MsgJobCrashed jobId
        doJob `catch` onException
      sendTo spc $ MsgUpdateRunningWithTid workerName tid -- async...risk...TODO
      reply rsvp $ ()
      handleWorkerMsg c (SPC spc) workerName
    MsgStopAndBreakLoop rsvp -> do -- should change into one RPC to SPC
    -- check status of the job 
      maybe_job_id <- requestReply spc $ MsgGetJobIdByWorkerName workerName  
      case maybe_job_id of
        Just jobId -> do
          jobCancel (SPC spc) jobId  -- job could be
          _ <- jobWait (SPC spc) jobId -- sync to make sure the job is removed
          workerGone (SPC spc) workerName -- remove workerName, sync
          reply rsvp $ () -- break the loop
        Nothing -> do 
          workerGone (SPC spc) workerName
          reply rsvp $ () -- no job to kill, just change state and break the loop

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts -- prior than schedule to make sure timeout expiration
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobId = spcJobCounter state
      modify $ \s -> s
          { spcJobsPending =
              (spcJobCounter s, job) : spcJobsPending s,
            spcJobCounter = JobId $ succ jobId
          }
      -- BEGIN debug
      -- state' <- get
      -- io $ putStrLn $ unlines 
      --   [
      --     "\tjob added " ++ show jobId,
      --     "\tPending " ++ (show (spcJobsPending state')),
      --     "\tCounter " ++ (show (spcJobCounter state'))
      --   ]
      io $ reply rsvp $ JobId jobId
    MsgJobStatus jobId rsvp -> do
      state <- get
      -- BEGIN debug
      -- state' <- get
      -- io $ putStrLn $ unlines 
      --   [
      --     "DEBUG IN JOB STATUS",
      --     "\tjob added " ++ show jobId,
      --     "\tPending " ++ (show (spcJobsPending state')),
      --     "\tRunning " ++ (show (spcJobsRunning state')),
      --     "\tDone " ++ (show (spcJobsDone state'))
      --     -- "\tCounter " ++ (show (spcJobCounter state'))
      --   ]
      io $ reply rsvp $ case ( lookup jobId $ spcJobsPending state,
                               lookup jobId $ spcJobsRunning state,
                               lookup jobId $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobId rsvp -> do
      -- io $ putStrLn $ "MsgJobWait"
      state <- get
      case lookup jobId $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ reason
        Nothing -> -- no reply, so it will wait. Here a job maybe both in wait list and running list
          modify $ \s -> s {spcWaiting = (jobId, rsvp) : spcWaiting s}
    MsgWorkerExists workerName rsvp -> do
      -- io $ putStrLn $ "MsgWorkerExists"
      exists <- workerExists workerName
      if exists then io $ reply rsvp $ True
      else io $ reply rsvp $ False
    MsgJobDoneByWorker jobId workerName rsvp -> do -- TODO could have concurrency conflict
      -- io $ putStrLn $ "MsgJobDoneByWorker"
      state <- get
      case lookup jobId $ spcJobsRunning state of 
        Just _ -> do -- TODO double check for workerName is the same?
          jobDone jobId (DoneByWorker workerName) -- return to the idle pool
          workerIsIdle
          io $ reply rsvp $ ()
          -- state' <- get
          -- io $ putStrLn $ unlines 
          --       [ "INSIDE MsgJobDoneByWorker:", 
          --         "job " ++ show jobId,
          --       "jobsDone:",
          --         show (spcJobsDone state'),
          --         "jobsRunning:",
          --         show (spcJobsRunning state') ]
        Nothing -> io $ reply rsvp $ ()
    -- update state inside the server
    MsgAddWorker workerName worker rsvp -> do
      state <- get
      let 
          updatedWorker = (workerName, worker) : spcWorkers state
          updatedWorkerIdle = (workerName) : spcWorkersIdle state -- init in the idle pool
      put $ state { 
          spcWorkers = updatedWorker,
          spcWorkersIdle = updatedWorkerIdle
        }
      io $ reply rsvp $ state
    MsgUpdateRunningWithTid workerName tid -> do -- TODO conflict state? 
      -- io $ putStrLn $ "MsgUpdateRunningWithTid"
      state <- get
      let updateJob (jobId, (deadline, maybe_tid, wName, job))
            | wName == workerName = (jobId, (deadline, Just tid, wName, job)) -- Update with new ThreadId
            | otherwise = (jobId, (deadline, maybe_tid, wName, job))
      modify $ \s -> s { spcJobsRunning = map updateJob (spcJobsRunning state) }
    MsgJobCancel cancel_jobId -> do
      -- io $ putStrLn $ "MsgJobCancel"
      state <- get
      case lookup cancel_jobId $ spcJobsRunning state of
        Just (_, Just tid, workerName, _) -> do
              io $ killThread tid
              jobDone cancel_jobId DoneCancelled -- Change the job state and then the worker state to make the system less busy.
              workerIsIdle
        _ -> pure () -- If the jobId is not referring to a running job, skip.
    MsgTick -> do 
      -- io $ putStrLn $ "MsgTick"
      pure ()
    MsgJobCrashed crashed_jobId -> do
      -- io $ putStrLn $ "MsgJobCrashed"
      state <- get
      case lookup crashed_jobId $ spcJobsRunning state of
        Just (_, Just tid, workerName, _) -> do
            io $ killThread tid
            jobDone crashed_jobId DoneCrashed
            workerIsIdle
        Just (_, Nothing, _, _) -> do -- not assigned tid yet... 
        -- wait for some time and retry... 
        -- for complement for the async assign of tid...
            io $ threadDelay 10
            io $ send c $ MsgJobCrashed crashed_jobId -- retry
        _ -> pure ()
    MsgGetJobIdByWorkerName workerName rsvp -> do
      -- io $ putStrLn $ "MsgGetJobIdByWorkerName"
      state <- get 
      let maybe_res = find (\(_, (_, _, wn, _)) -> wn == workerName) (spcJobsRunning state)
      case maybe_res of 
        Nothing -> 
            io $ reply rsvp $ Nothing 
        Just (jobId, (_, _, _, _)) -> 
            io $ reply rsvp $ Just jobId 
    MsgRemoveWorker workerName rsvp -> do 
        workerIsGone

      io $ reply rsvp $ ()
    MsgRemoveWorkersJobIfAnyRunning workerName rsvp -> do -- less async but more safety
      state <- get 
      let maybe_res = find (\(_, (_, _, wn, _)) -> wn == workerName) (spcJobsRunning state)
      case maybe_res of -- no running job, exit
        Nothing -> 
          io $ reply rsvp $ () 
        Just (jobId, (_, _, _, _)) -> do -- has running jobs
          case lookup cancel_jobId $ spcJobsRunning state of -- cancel job
            Just (_, Just tid, workerName, _) -> do
              io $ killThread tid
              jobDone cancel_jobId DoneCancelled
              workerIsIdle
            _ -> pure () -- If the jobId is not referring to a running job, skip.
          workerIsGone
          io $ reply rsvp $ ()



startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcChan = c,
            spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcWaiting = [],
            spcJobsDone = [],
            spcWorkers = [],
            spcWorkersIdle = []
          }
  c <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer c -- detach a thread for timer, send to MsgTick per 1sec
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 10000 -- 10ms tick
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Synchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobId =
  requestReply c $ MsgJobStatus jobId

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobId =
  requestReply c $ MsgJobWait jobId

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobId =
  sendTo c $ MsgJobCancel jobId

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) workerName = do
  exists <- requestReply c $ MsgWorkerExists workerName -- sync
  if exists
    then pure $ Left "WorkerName already exists"
    else do
      wc <- spawn $ \chan -> handleWorkerMsg chan (SPC c) workerName -- async
      _ <- requestReply c $ MsgAddWorker workerName (Worker wc) --sync
      pure $ Right $ Worker wc

-- | Shut down a running worker. No effect if the worker is already
-- terminated. syc.
workerStop :: Worker -> IO ()
workerStop (Worker w) = 
  requestReply w $ MsgStopAndBreakLoop

workerGone :: SPC -> WorkerName -> IO()
workerGone (SPC c) workerName = do 
  requestReply c $ MsgRemoveWorker workerName

-- data Worker = Worker (Server WorkerMsg)
