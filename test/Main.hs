import Path
import System.INotify (initINotify, removeWatch, killINotify)
import System.File.Follow (follow)
import System.Directory (getCurrentDirectory)
import Control.Monad (forever)
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)


main = do
  i <- initINotify
  d <- getCurrentDirectory
  f <- parseAbsFile $ d ++ "/foo"
  bracket (follow i f print) (\watch -> do
                                removeWatch watch
                                killINotify i
                              ) $ \_ -> forever $ threadDelay 50000
