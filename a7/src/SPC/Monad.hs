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
