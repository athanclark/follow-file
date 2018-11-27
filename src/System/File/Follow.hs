{-# LANGUAGE
    ScopedTypeVariables
  , NamedFieldPuns
  , TupleSections
  , Rank2Types
  , FlexibleContexts
  #-}

module System.File.Follow where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Text.Encoding as T
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Attoparsec.Path (relFilePath)
import Data.Conduit (ConduitT, yield)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.Trans.Control (MonadBaseControl (liftBaseWith))
import Path (Path, Abs, File, filename, parent, toFilePath)
import System.Posix.IO.ByteString (fdReadBuf, openFd, OpenMode (ReadOnly), defaultFileFlags, closeFd, fdSeek)
import System.Posix.Types (FileOffset)
import System.Posix.ByteString.FilePath (RawFilePath)
import System.Posix.Files.ByteString (fileSize, getFileStatus)
import System.Directory (doesFileExist)
import System.INotify (INotify, addWatch, Event (..), EventVariety (..), WatchDescriptor)
import GHC.IO.Device (SeekMode (AbsoluteSeek))


-- | 'follow' takes a file, and informs you /only/ when it changes. If it's deleted,
-- | you're notified with an empty 'Data.ByteString.ByteString'. If it doesn't exist yet, you'll be informed
-- | of its entire contents upon it's creation, and will proceed to "follow it" as normal.
follow :: ( MonadIO m
          , MonadMask m
          , MonadBaseControl IO m
          )
       => INotify
       -> Path Abs File
       -> (ConduitT i BS.ByteString m () -> m ()) -- ^ Monadic state of @m@ is thrown away for each invocation, not synchronously interleaved.
       -> m WatchDescriptor
follow inotify file f = do
  let file' = toFilePath file
  exists <- liftIO (doesFileExist file')
  (positionRef :: IORef FileOffset) <- liftIO $
    if exists
      then getFileStatus (BS8.fromString file') >>= (newIORef . fileSize)
      else newIORef 0
  let go  = bracket (liftIO $ openFd (BS8.fromString file') ReadOnly Nothing defaultFileFlags)
                    (liftIO . closeFd) $ \fd -> do
              toSeek <- liftIO (readIORef positionRef)
              idx <- liftIO (fdSeek fd AbsoluteSeek toSeek)
              liftIO (writeIORef positionRef idx)
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
        liftIO (writeIORef positionRef 0)
        f (yield mempty)
  liftBaseWith $ \runInBase -> addWatch inotify [Modify, Create, Delete] (BS8.fromString $ toFilePath $ parent file) $ \e ->
    let isFile :: RawFilePath -> Bool
        isFile filePath = parseOnly (relFilePath <* endOfInput) (T.decodeUtf8 filePath) == Right (filename file)
    in  case e of
          Created {filePath}
            | isFile filePath -> void $ runInBase go
            | otherwise -> pure ()
          Deleted {filePath}
            | isFile filePath -> void $ runInBase stop
            | otherwise -> pure ()
          Modified {maybeFilePath}
            | (isFile <$> maybeFilePath) == Just True -> void $ runInBase go
            | otherwise -> pure ()
          MovedIn {filePath}
            | isFile filePath -> void $ runInBase go
            | otherwise -> pure ()
          MovedOut {filePath}
            | isFile filePath -> void $ runInBase go
            | otherwise -> pure ()
          DeletedSelf -> error "containing folder deleted"
          Unmounted -> error "containing folder unmounted"
          QOverflow -> error "queue overflow"
          _ -> pure ()
