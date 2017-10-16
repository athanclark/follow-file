{-# LANGUAGE
    ScopedTypeVariables
  , NamedFieldPuns
  , TupleSections
  , Rank2Types
  #-}

module System.File.Follow where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Attoparsec.Path (relFilePath)
import Data.Conduit (Producer, yield)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import Path (Path, Abs, File, filename, parent, toFilePath, parseRelFile)
import System.Posix.IO.ByteString (fdReadBuf, openFd, OpenMode (ReadOnly), defaultFileFlags, closeFd, fdSeek)
import System.Posix.Types (FileOffset)
import System.Posix.Files.ByteString (fileSize, getFileStatus)
import System.Directory (doesFileExist)
import System.INotify (INotify, addWatch, Event (..), EventVariety (..), WatchDescriptor)
import GHC.IO.Device (SeekMode (AbsoluteSeek))


-- | 'follow' takes a file, and informs you /only/ when it changes. If it's deleted,
-- | you're notified with an empty 'Data.ByteString.ByteString'. If it doesn't exist yet, you'll be informed
-- | of its entire contents upon it's creation, and will proceed to "follow it" as normal.
follow :: INotify
        -> Path Abs File
        -> (Producer IO BS.ByteString -> IO ())
        -> IO WatchDescriptor
follow inotify file f = do
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
              let loop = do
                    c <- liftIO $ BS.createUptoN LBS.defaultChunkSize $ \ptr -> do
                      seeked <- readIORef positionRef
                      moreRead <- fdReadBuf fd ptr (fromIntegral LBS.defaultChunkSize)
                      writeIORef positionRef (seeked + fromIntegral moreRead)
                      pure (fromIntegral moreRead)
                    if c == mempty
                      then pure ()
                      else do
                        yield c
                        loop
              f loop
      stop = do
        writeIORef positionRef 0
        f (yield mempty)
  addWatch inotify [Modify, Create, Delete] (toFilePath $ parent file) $ \e ->
    let isFile filePath = parseOnly (relFilePath <* endOfInput) (T.pack filePath) == Right (filename file)
    in  case e of
          Created {filePath}  | isFile filePath -> go
                              | otherwise -> pure ()
          Deleted {filePath}  | isFile filePath -> stop
                              | otherwise -> pure ()
          Modified {maybeFilePath}  | (isFile <$> maybeFilePath) == Just True -> go
                                    | otherwise -> pure ()
          MovedIn {filePath}  | isFile filePath -> go
                              | otherwise -> pure ()
          MovedOut {filePath}   | isFile filePath -> go
                                | otherwise -> pure ()
          DeletedSelf -> error "containing folder deleted"
          Unmounted -> error "containing folder unmounted"
          QOverflow -> error "queue overflow"
          _ -> pure ()
