module SPC.InterpPure
  (
    interpCCPure
  )
where

import SPC.Monad
import Data.Maybe(fromMaybe)


-- pure SPC 
type ChanId = Int

data CCState = CCState
  { ccCounter :: ChanId,
    ccChans :: [(ChanId, [Msg])],
    ccThreads :: [CC ChanId ()]
  }

getChan :: ChanId -> State CCState [Msg]
getChan chan_id = do
  state <- get
  pure $
    fromMaybe (error "unknown channel") $
      lookup chan_id $
        ccChans state

setChan :: ChanId -> [Msg] -> State CCState ()
setChan chan_id msgs = do
  state <- get
  put $
    state
      { ccChans =
          (chan_id, msgs)
            : filter ((/= chan_id) . fst) (ccChans state)
      }

addThread :: CC ChanId () -> State CCState ()
addThread m = do
  state <- get
  put $ state {ccThreads = m : ccThreads state}

incCounter :: State CCState ChanId
incCounter = do
  state <- get
  put $ state {ccCounter = ccCounter state + 1}
  pure $ ccCounter state

-- simulation of "forever loop"
step :: CC Int a -> State CCState (CC ChanId a)
step (Pure x) = pure $ Pure x
step (Free (CCNewChan c)) = do
  chan_id <- incCounter
  setChan chan_id []
  step $ c chan_id

step (Free (CCFork m c)) = do
  addThread m
  step c

step (Free (CCSend chan_id msg c)) = do
  msgs <- getChan chan_id
  setChan chan_id $ msgs ++ [msg]
  pure c -- simulate block

step (Free (CCReceive chan_id c)) = do
  msgs <- getChan chan_id
  case msgs of
    [] -> pure $ Free $ CCReceive chan_id c
    msg : msgs' -> do
      setChan chan_id msgs'
      step $ c msg

-- A correct solution requires is to 
-- remove the threads from the state before we step them:
stepThreads :: State CCState ()
stepThreads = do
  state <- get
  put $ state {ccThreads = []}
  threads <- mapM step $ ccThreads state
  new_state <- get
  put $ new_state {ccThreads = threads ++ ccThreads new_state}

interp :: CC ChanId a -> State CCState a
interp (Pure x) = pure x
interp (Free op) = do
  stepThreads
  op' <- step $ Free op
  interp op'

interpCCPure :: CC ChanId a -> a
interpCCPure orig =
  fst $ runState initial_state $ interp orig
  where
    initial_state =
      CCState
        { ccCounter = 0,
          ccChans = [],
          ccThreads = []
        }
