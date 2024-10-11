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

import Data.List(partition)
import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
  )
import Control.Monad (ap, forever, liftM, void, forM_)
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
data WorkerMsg -- TODO: add messages.
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
  | -- | get current state through IO
    MsgGetCurrentState (ReplyChan SPCState)
  | MsgJobDoneByWorker JobId WorkerName
  | MsgAddWorker  WorkerName Worker (ReplyChan SPCState)

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker (Server WorkerMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcChan :: Chan SPCMsg, -- a handler for worker thread access 
    spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (JobDoneReason))],
    spcJobCounter :: JobId,
    -- TODO: you will need to add more fields.
    spcWorkers :: [(WorkerName, Worker)],
    spcWorkersIdle :: [WorkerName]
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
-- TODO will schedule be changing the state with other thread?
schedule :: SPCM ()
schedule = do
  state <- get
  case (spcWorkersIdle state, spcJobsPending state) of
    (workerName : idleWorkers, (jobId, job) : pendingJobs) -> do
      -- Find the worker server, handle the case where the worker might not exist
      case lookup workerName (spcWorkers state) of
        Just (Worker workerServer) -> do
          -- Update the state with the new list of idle workers and running jobs
          let updatedWorkers = idleWorkers
              updatedRunningJobs = (jobId, job) : spcJobsRunning state
          put $
            state
              { spcWorkersIdle = updatedWorkers,
                spcJobsPending = pendingJobs,
                spcJobsRunning = updatedRunningJobs
              }
          -- Send the job to the worker (non-blocking, asynchronous)
          io $ sendTo workerServer (MsgAssignJob job jobId)
        Nothing -> pure ()  -- Handle the case where the worker wasn't found, should never reach here
    _ -> pure ()  -- No idle workers or no pending jobs, do nothing

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case lookup jobid $ spcJobsDone state of
    Just _ ->
      -- We already know this job is done.
      pure ()
    Nothing -> do
      let (waiting_for_job, not_waiting_for_job) =
            partition ((== jobid) . fst) (spcWaiting state)
      forM_ waiting_for_job $ \(_, rsvp) ->
        io $ reply rsvp $ reason
      put $
        state
          { spcWaiting = not_waiting_for_job,
            spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcJobsPending = removeAssoc jobid $ spcJobsPending state
          }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle = undefined

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = pure () -- change in Task 4

workerExists :: WorkerName -> SPCM Bool
workerExists workerName = do
  state <- get
  pure $ any ((== workerName) . fst) (spcWorkers state)

handleWorkerMsg :: Chan WorkerMsg -> WorkerName -> SPCM ()
handleWorkerMsg c workerName = forever $ do
  schedule -- TODO also schedule? 
  msg <- io $ receive c
  case msg of
    MsgAssignJob job jobId -> do
      state <- get
      io $ do -- save thread resource, not using forkIO
          jobAction job -- simulate the job action with a data def
          send (spcChan state) $ MsgJobDoneByWorker jobId workerName
      modify $ \state ->
        state { spcWorkersIdle = workerName : spcWorkersIdle state }
      pure ()

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason -> do
          io $ reply rsvp $ reason
        Nothing ->
          put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}
    MsgWorkerExists workerName rsvp -> do
      exists <- workerExists workerName
      if exists then io $ reply rsvp $ True
      else io $ reply rsvp $ False
    MsgGetCurrentState rsvp -> do 
      state <- get 
      io $ reply rsvp $ state
    MsgJobDoneByWorker jobid workerName -> do
      state <- get
      case lookup jobid $ spcJobsRunning state of
        Just _ -> do
          jobDone jobid (DoneByWorker workerName)
        Nothing -> pure ()
    -- update state inside the server
    MsgAddWorker workerName worker rsvp -> do
      state <- get
      let 
          updatedWorker = (workerName, worker) : spcWorkers state
          updatedWorkerIdle = (workerName) : spcWorkersIdle state
      put $ state { 
          spcWorkers = updatedWorker,
          spcWorkersIdle = updatedWorkerIdle
        }
      io $ reply rsvp $ state


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
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) name = do
  exists <- requestReply c $ MsgWorkerExists name -- sync
  if exists
    then pure $ Left "WorkerName already exists"
    else do
      state <- requestReply c $ MsgGetCurrentState  -- sync
      wc <- spawn $ \chan -> runSPCM state $ handleWorkerMsg chan name -- async
      _ <- requestReply c $ MsgAddWorker name (Worker wc) --sync
      pure $ Right $ Worker wc

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop = undefined