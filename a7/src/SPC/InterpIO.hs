module SPC.InterpIO
  ( 
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
import SPC.Monad

-- IO SPC
-- interpCCIO :: CC (Chan Msg) a -> IO a
interpCCIO :: CC (Chan Msg) a -> IO a
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
