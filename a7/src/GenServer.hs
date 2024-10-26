module GenServer
  ( 
    pipeline,
    interpCCIO
  )
where

import Control.Concurrent
  ( forkIO,
    Chan,
    newChan,
    writeChan,
    readChan
  )
import Control.Monad (ap)

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

data CCOp a
  = CCFork (CC ()) a
  | CCNewChan (Chan Msg -> a)
  | CCSend (Chan Msg) Msg a
  | CCReceive (Chan Msg) (Msg -> a)

type CC a = Free CCOp a

instance Functor CCOp where
  fmap f (CCFork m c) = CCFork m (f c)
  fmap f (CCNewChan c) = CCNewChan $ f . c
  fmap f (CCSend chan msg c) = CCSend chan msg $ f c
  fmap f (CCReceive chan c) = CCReceive chan $ f . c

-- interpCCIO :: CC (Chan Msg) a -> IO a
interpCCIO :: CC a -> IO a
interpCCIO (Pure x) =
  pure x
interpCCIO (Free (CCFork m c)) = do
  _ <- forkIO $ interpCCIO m
  interpCCIO c
interpCCIO (Free (CCNewChan c)) = do
  chan <- newChan
  interpCCIO $ c chan
interpCCIO (Free (CCSend chan msg c)) = do
  writeChan chan msg
  interpCCIO c
interpCCIO (Free (CCReceive chan c)) = do
  msg <- readChan chan
  interpCCIO $ c msg

ccNewChan :: CC (Chan Msg)
ccNewChan = Free $ CCNewChan pure

ccFork :: CC () -> CC ()
ccFork m = Free $ CCFork m $ pure ()

ccSend :: Chan Msg -> Msg -> CC ()
ccSend chan msg = Free $ CCSend chan msg $ pure ()

ccReceive :: Chan Msg -> CC Msg
ccReceive chan = Free $ CCReceive chan pure

pipeline :: CC String
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