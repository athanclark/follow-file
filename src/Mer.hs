module Main where


import System.INotify
import System.Environment (getArgs)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Concurrent.STM.TVar
import Control.Monad (forM_)


main :: IO ()
main = do
  [file] <- getArgs


  -- make changes every 1/10th of a second for 10 seconds
  forkIO $ forM_ [0..100] $ \s -> do
    appendFile file $ show s
    threadDelay (second `div` 10)


  debouncer <- atomically $ newTSem 0
  notif <- initINotify
  expectation <- newTVarIO (0 :: Int)

  watcher <- addWatch notif [Modify] file $ \e -> do
    e' <- atomically $ do
      modifyTVar expectation (+1)
      readTVar expectation
    print e
    threadDelay second
    e'' <- readTVarIO expectation
    if e' == e''
    then atomically $ signalTSem debouncer
    else pure ()

  atomically $ waitTSem debouncer
  removeWatch watcher
  killINotify notif


second = 1000000
