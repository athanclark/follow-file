module Main where


import System.INotify
import System.Environment (getArgs)
import Data.IORef
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad (forM_)


main :: IO ()
main = do
  [file] <- getArgs


  -- make changes every 1/10th of a second for 10 seconds
  forkIO $ forM_ [0..100] $ \s -> do
    appendFile file $ show s
    threadDelay (second `div` 10)


  debouncer <- atomically $ newTSem 0

  watcher <- withINotify $ \notif -> addWatch notif [Modify] file $ \e -> do
    print e
    threadDelay second
    atomically $ signalTSem debouncer

  atomically $ waitTSem debouncer
  removeWatch watcher



second = 1000000
