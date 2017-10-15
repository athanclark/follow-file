{-# LANGUAGE
    ScopedTypeVariables
  , NamedFieldPuns
  , TupleSections
  #-}

module System.File.Neglect where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Exception (bracket)
import Path (Path, Abs, File, filename, parent, toFilePath, parseRelFile)
import System.Posix.IO.ByteString (fdReadBuf, openFd, OpenMode (ReadOnly), defaultFileFlags, closeFd, fdSeek)
import System.Posix.Types (FileOffset)
import System.Posix.Files.ByteString (fileSize, getFileStatus)
import System.Directory (doesFileExist)
import System.INotify (INotify, addWatch, Event (..), EventVariety (..), WatchDescriptor)
import GHC.IO.Device (SeekMode (AbsoluteSeek))


-- | Neglect takes a file, and only informs you when it changes. If it's deleted,
-- | you're informed that nothing changed. If it doesn't exist yet, you'll be informed
-- | on it's creation.
neglect :: INotify
        -> Path Abs File
        -> (LBS.ByteString -> IO ())
        -> IO WatchDescriptor
neglect inotify file f = do
  let file' = toFilePath file
  exists <- doesFileExist file'
  (positionRef :: IORef FileOffset) <-
    if exists
      then getFileStatus (BS8.fromString file') >>= (newIORef . fileSize)
      else newIORef 0
  let go  = bracket (openFd (BS8.fromString file') ReadOnly Nothing defaultFileFlags)
                    closeFd $ \fd -> do
              toSeek <- readIORef positionRef
              idx <- fdSeek fd AbsoluteSeek toSeek
              writeIORef positionRef idx
              let loop acc = do
                    c <- BS.createUptoN LBS.defaultChunkSize $ \ptr -> do
                      seeked <- readIORef positionRef
                      moreRead <- fdReadBuf fd ptr (fromIntegral LBS.defaultChunkSize)
                      writeIORef positionRef (seeked + fromIntegral moreRead)
                      pure (fromIntegral moreRead)
                    if c == mempty
                      then pure acc
                      else loop (acc `V.snoc` c)
              theRest <- loop V.empty
              when (theRest /= V.empty) (f (foldr LBS.chunk mempty theRest))
      stop = do
        writeIORef positionRef 0
        f mempty
  addWatch inotify [Modify, Create, Delete] (toFilePath $ parent file) $ \e -> case e of
    Created {filePath} | parseRelFile filePath == Just (filename file) -> go
                       | otherwise -> pure ()
    Deleted {filePath} | parseRelFile filePath == Just (filename file) -> stop
                       | otherwise -> pure ()
    Modified {maybeFilePath} | ( parseRelFile =<< maybeFilePath
                               ) == Just (filename file) -> go
                             | otherwise -> pure ()
    _ -> pure ()
