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
import Control.Monad (ap, forever, liftM, void, forM_)
import Data.List(partition)
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

-- Then the definition of the glorious SPC.

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

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
 = MsgAssignJob Job JobId

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
  | -- | get SpcChan only. No other states!
    MsgGetSpcChannel (ReplyChan (Chan SPCMsg))
  | MsgJobDoneByWorker JobId WorkerName
  | MsgAddWorker  WorkerName Worker (ReplyChan SPCState)
  | MsgUpdateRunningWithTid WorkerName ThreadId

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, (Seconds, Maybe ThreadId, WorkerName))],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (JobDoneReason))],
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
  case (spcWorkersIdle state, spcJobsPending state) of
    (workerName : idleWorkers, (jobId, job) : pendingJobs) -> do
      case lookup workerName (spcWorkers state) of
        Just (Worker workerServer) -> do
          now <- io $ getSeconds
          -- Update the state with the new list of idle workers and running jobs
          let deadline = now + fromIntegral (jobMaxSeconds job)
              updatedRunningJobs = (jobId, (deadline, Nothing, workerName)) : spcJobsRunning state
          modify $ \s -> s
              { spcWorkersIdle = idleWorkers, -- leave the idle pool
                spcJobsPending = pendingJobs,
                spcJobsRunning = updatedRunningJobs
              }
          -- Send the job to the worker (non-blocking, asynchronous)
          io $ sendTo workerServer (MsgAssignJob job jobId)
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

-- TODO question can we leave workerIsIdle there...?
-- TODO defined but not used...and modified from the returning SPCM ()
-- workerIsIdle :: WorkerName -> Worker -> SPCM Bool
-- workerIsIdle workerName worker = do
--   state <- get
--   pure $  workerName `elem` spcWorkersIdle state

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

-- guarantee no state change while doing checkTimeout, thread safe.
checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  now <- io getSeconds
  -- io $ putStrLn $ "[checkTimeouts] current_time=" ++ show now
  let runningJobs = spcJobsRunning state
      (timedOutJobs, activeJobs) = 
          partition (\(_, (deadline, maybe_tid, _)) -> now >= deadline) runningJobs

  forM_ timedOutJobs $ \(jobId, ( _, maybe_tid, workerName)) -> 
    case maybe_tid of
      Just tid -> do
        io $ killThread tid 
        jobDone jobId DoneTimeout -- chage jobs state -- TODO add a batch jobDone to reduce concurrency
        modify $ \s -> s { spcWorkersIdle = workerName : spcWorkersIdle s }
      Nothing -> do
        io $ threadDelay 10 -- TODO magic number
        checkTimeouts

workerExists :: WorkerName -> SPCM Bool
workerExists workerName = do
  state <- get
  pure $ any ((== workerName) . fst) (spcWorkers state)

-- only do IO no state change. only state change in SPC in sequence.
handleWorkerMsg :: Chan WorkerMsg -> Chan SPCMsg -> WorkerName -> IO ()
handleWorkerMsg c spc_ch workerName = forever $ do
  msg <- receive c
  case msg of
    MsgAssignJob job jobId -> do
      tid <- forkIO $ do -- this thread id is not the same as the worker thread id...
          jobAction job
          send spc_ch $ MsgJobDoneByWorker jobId workerName -- when the job is canceled, the MsgJobDoneByWorker may not be sent
      send spc_ch $ MsgUpdateRunningWithTid workerName tid -- tell SPC the worker in charge for tid
      pure ()

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
      io $ reply rsvp $ JobId jobId
    MsgJobStatus jobId rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobId $ spcJobsPending state,
                               lookup jobId $ spcJobsRunning state,
                               lookup jobId $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobId rsvp -> do
      state <- get
      case lookup jobId $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ reason
        Nothing -> -- no reply, so it will wait. Here a job maybe both in wait list and running list
          modify $ \s -> s {spcWaiting = (jobId, rsvp) : spcWaiting s}
    MsgWorkerExists workerName rsvp -> do
      exists <- workerExists workerName
      if exists then io $ reply rsvp $ True
      else io $ reply rsvp $ False
    MsgGetSpcChannel rsvp -> do 
      state <- get 
      io $ reply rsvp $ (spcChan state)
    MsgJobDoneByWorker jobId workerName -> do -- TODO could have concurrency conflict
      state <- get
      case lookup jobId $ spcJobsRunning state of 
        Just _ -> do -- TODO double check for workerName is the same?
          jobDone jobId (DoneByWorker workerName) -- return to the idle pool
          modify $ \s -> s { spcWorkersIdle = workerName : spcWorkersIdle state} 
          -- state' <- get
          -- io $ putStrLn $ unlines 
          --       [ "INSIDE MsgJobDoneByWorker:", 
          --         "job " ++ show jobId,
          --       "jobsDone:",
          --         show (spcJobsDone state'),
          --         "jobsRunning:",
          --         show (spcJobsRunning state') ]
        Nothing -> pure () 
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
      state <- get
      let updatedJobsRunning = map updateJob (spcJobsRunning state)
          updateJob (jobId, (deadline, maybe_tid, wName))
            | wName == workerName = (jobId, (deadline, Just tid, wName)) -- Update with new ThreadId
            | otherwise = (jobId, (deadline, maybe_tid, wName))
      put state { spcJobsRunning = updatedJobsRunning }
    MsgJobCancel cancel_jobId -> do
      state <- get
      case lookup cancel_jobId $ spcJobsRunning state of
        Just (_, Just tid, workerName) -> do
              io $ killThread tid
              jobDone cancel_jobId DoneCancelled -- Change the job state and then the worker state to make the system less busy.
              modify $ \s -> s { spcWorkersIdle = workerName : spcWorkersIdle state }
        _ -> pure () -- If the jobId is not referring to a running job, skip.
    MsgTick -> pure ()
      -- maybe in decentralized way for task timeout,
      -- we should Tick to Worker rather than server
      -- why we tick to server here? 

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
      threadDelay 1000000
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
workerAdd (SPC c) name = do
  exists <- requestReply c $ MsgWorkerExists name -- sync
  if exists
    then pure $ Left "WorkerName already exists"
    else do
      spc_ch <- requestReply c $ MsgGetSpcChannel 
      wc <- spawn $ \chan -> handleWorkerMsg chan spc_ch name -- async
      _ <- requestReply c $ MsgAddWorker name (Worker wc) --sync
      pure $ Right $ Worker wc

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop = undefined