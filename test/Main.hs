import Path
import System.INotify (initINotify, removeWatch, killINotify)
import System.File.Neglect (neglect)
import Control.Monad (forever)
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)


main = do
  i <- initINotify
  f <- parseAbsFile "/home/athan/dev/fileneglect/foo"
  bracket (neglect i f print) (\watch -> do
                                removeWatch watch
                                killINotify i
                              ) $ \_ -> forever $ threadDelay 50000
