module SPC.Monad
  ( 
    Free (..),
    CCOp (..),
    CC,
    Msg,
    State,
    runState,
    put,
    get
  )
where
import Control.Monad (ap, forever, liftM)

-- ANCHOR: State
newtype State s a = State (s -> (a, s))

-- ANCHOR_END: State

-- ANCHOR: Functor_State
instance Functor (State s) where
  fmap = liftM

-- ANCHOR_END: Functor_State

-- ANCHOR: Applicative_State
instance Applicative (State s) where
  pure x = State $ \state -> (x, state)
  (<*>) = ap

-- ANCHOR_END: Applicative_State

-- ANCHOR: Monad_State
instance Monad (State s) where
  State m >>= f = State $ \state ->
    let (x, state') = m state
        State f' = f x
     in f' state'

-- ANCHOR_END: Monad_State

runState :: s -> State s a -> (a, s)
runState s (State f) = f s

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)


-- Free Monad
type Msg = String

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data CCOp chan a
  = CCFork (CC chan ()) a
  | CCNewChan (chan -> a)
  | CCSend chan Msg a
  | CCReceive chan (Msg -> a)

instance Functor (CCOp chan) where
  fmap f (CCFork m c) = CCFork m (f c)
  fmap f (CCNewChan c) = CCNewChan $ f . c
  fmap f (CCSend chan msg c) = CCSend chan msg $ f c
  fmap f (CCReceive chan c) = CCReceive chan $ f . c

type CC chan a = Free (CCOp chan) a


ccNewChan :: CC chan chan
ccNewChan = Free $ CCNewChan pure

ccFork :: CC chan () -> CC chan ()
ccFork m = Free $ CCFork m $ pure ()

ccSend :: chan -> Msg -> CC chan ()
ccSend chan msg = Free $ CCSend chan msg $ pure ()

ccReceive :: chan -> CC chan Msg
ccReceive chan = Free $ CCReceive chan pure

pipeline :: CC chan String
pipeline = do
  chan_0 <- ccNewChan
  chan_1 <- ccNewChan
  chan_2 <- ccNewChan
  chan_3 <- ccNewChan
  chan_4 <- ccNewChan
  let passOn tok from to = do
        x <- ccReceive from
        ccSend to $ x ++ tok
  ccFork $ passOn "a" chan_0 chan_1
  ccFork $ passOn "b" chan_1 chan_2
  ccFork $ passOn "c" chan_2 chan_3
  ccFork $ passOn "d" chan_3 chan_4
  ccSend chan_0 ""
  ccReceive chan_4

infiniteWrite :: CC chan String
infiniteWrite = do
  chan <- ccNewChan
  ccFork $ forever $ ccSend chan "x"
  a <- ccReceive chan
  b <- ccReceive chan
  pure $ a ++ b

-- limitation of simulating multi thread in Monad:
-- when interpreting a free monad, 
-- the only time we can "interrupt" computation and get back control is when an effect occurs, 
-- so there is no way we can avoid this problem in interpCCPure
-- =>XXX Haskell does not allow us to inject effects into otherwise pure code. XXX<=
infiniteLoop :: CC chan String
infiniteLoop = do
  chan <- ccNewChan
  ccFork $ forever $ pure () -- no effect so nothing will go downstream...
  ccFork $ ccSend chan "x"
  ccReceive chan
