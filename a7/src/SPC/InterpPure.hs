module SPC.InterpPure
  (
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
  step c

step (Free (CCReceive chan_id c)) = do
  msgs <- getChan chan_id
  case msgs of
    [] -> pure $ Free $ CCReceive chan_id c
    msg : msgs' -> do
      setChan chan_id msgs'
      step $ c msg
