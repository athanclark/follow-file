{-# LANGUAGE
    ScopedTypeVariables
  #-}

module System.File.Neglect where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal as BS
import Control.Monad (void)
import Control.Exception (bracket)
import Path (Path, Abs, File, parent, toFilePath)
import System.FSNotify (withManager, watchDir, Event (..))
import System.Posix.IO (fdReadBuf, openFd, OpenMode (ReadOnly), defaultFileFlags, closeFd, fdSeek)
import System.Posix.Types (FileOffset)
import System.Posix.Files (fileSize, getFileStatus)
import System.Directory (doesFileExist)
import GHC.IO.Device (SeekMode (AbsoluteSeek))


-- | Neglect takes a file, and only informs you when it changes. If it's deleted,
-- | you're informed that nothing changed. If it doesn't exist yet, you'll be informed
-- | on it's creation.
neglect :: Path Abs File
        -> (LBS.ByteString -> IO ())
        -> IO ()
neglect file f = do
  let file' = toFilePath file
  exists <- doesFileExist file'
  (positionRef :: IORef FileOffset) <-
    if exists
      then getFileStatus file' >>= (newIORef . fileSize)
      else newIORef 0
  let go  = bracket (openFd file' ReadOnly Nothing defaultFileFlags)
                    closeFd $ \fd -> do
              toSeek <- readIORef positionRef
              idx <- fdSeek fd AbsoluteSeek (toSeek + fromIntegral LBS.defaultChunkSize)
              writeIORef positionRef idx
              let loop acc = do
                    c <- BS.createUptoN (fromIntegral LBS.defaultChunkSize) $ \ptr ->
                      fromIntegral <$> fdReadBuf fd ptr (fromIntegral LBS.defaultChunkSize)
                    if c == mempty
                      then pure mempty
                      else loop (LBS.chunk c acc)
              theRest <- loop mempty
              f theRest
      stop = do
        writeIORef positionRef 0
        f mempty
  void $ withManager $ \mgr ->
    watchDir
      mgr
      (toFilePath $ parent file)
      (const True) $ \e ->
        case e of
          Added file'' _
            | file'' == file' -> go
            | otherwise -> pure ()
          Modified file'' _
            | file'' == file' -> go
            | otherwise -> pure ()
          Removed file'' _
            | file'' == file' -> stop
            | otherwise -> pure ()
