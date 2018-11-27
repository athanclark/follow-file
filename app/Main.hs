module Main where

import Path
import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly, endOfInput, eitherP)
import Data.Attoparsec.Path (absFilePath, relFilePath, absDirPath)
import Data.Conduit ((.|), runConduit)
import Data.Conduit.Combinators (stdout)
import System.INotify (initINotify, removeWatch, killINotify)
import System.File.Follow (follow)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import Control.Monad (forever)
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
  [filePath] <- getArgs
  f <- case parseOnly (eitherP absFilePath relFilePath <* endOfInput) (T.pack filePath) of
          Left e -> error e
          Right eAR -> case eAR of
            Left a -> pure a
            Right r -> do
              d <- getCurrentDirectory
              case parseOnly (absDirPath <* endOfInput) (T.pack (d ++ "/")) of
                Left e -> error e
                Right d' ->
                  pure (d' </> r)
  i <- initINotify
  let obtain = follow i f $ \source -> runConduit (source .| stdout)
      release watch = removeWatch watch >> killINotify i
  bracket obtain release $ \_ ->
    forever (threadDelay 50000) -- run forever
