{-|
Module      : System.IDontNotify
Copyright   : (c) 2016  Athan Clark
License     : BSD-style
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC
-}

module System.IDontNotify where

import System.INotify
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad (when, void)


-- | Block execution until a file stops misbehaving:
--
-- > neglectFile "foo" 1000000
--
-- will wait until the modifications to file @foo@ happen in an interval greater
-- than one second.
neglectFile :: FilePath -- ^ File to watch
            -> Int -- ^ Decay threshold in picoseconds
            -> IO ()
neglectFile file time = do
  debouncer   <- atomically $ newTSem 0
  notif       <- initINotify
  expectation <- newTVarIO (0 :: Int)

  watcher <- addWatch notif [Modify] file $ \e -> void $ forkIO $ do
    e' <- atomically $ do
      modifyTVar expectation (+1)
      readTVar expectation
    print e
    threadDelay time
    e'' <- readTVarIO expectation
    when (e' == e'') $
      atomically $ signalTSem debouncer

  atomically $ waitTSem debouncer
  removeWatch watcher
  killINotify notif
