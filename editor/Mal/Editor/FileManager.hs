module Mal.Editor.FileManager
(
    Handle
  , getFileContents
  , getFileName
  , hasFileChanged
  , new
  , openFile
  , reset
  , save
  , setFileContents
)
where

import           Crypto.Hash
import           Data.IORef
import           Data.Text          (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO       as TIO

data Handle =
  MkFileManager
  { _fmFileName     :: IORef FilePath
  , _fmFileContents :: IORef Text
  , _fmFileHash     :: IORef (Digest SHA1)
  }

defaultFileName :: String
defaultFileName = "New File"

-- | Create a new file manager handle from a given @FilePath@.
new :: Maybe FilePath -> IO Handle
new (Just filename) = do
  fileContents <- TIO.readFile filename
  MkFileManager
    <$> newIORef filename
    <*> newIORef fileContents
    <*> newIORef (hashContents fileContents)
new Nothing =
  MkFileManager
    <$> newIORef defaultFileName
    <*> newIORef ""
    <*> newIORef (hashContents "")

reset :: Handle -> IO ()
reset fm = do
  writeIORef (_fmFileName fm) defaultFileName
  writeIORef (_fmFileContents fm) ""
  writeIORef (_fmFileHash fm) (hashContents "")

-- | Get the current file name of the file manager.
getFileName :: Handle -> IO FilePath
getFileName = readIORef . _fmFileName

-- | Get the current file's contents.
getFileContents :: Handle -> IO Text
getFileContents = readIORef . _fmFileContents

setFileContents :: Handle -> Text -> IO ()
setFileContents fm = writeIORef (_fmFileContents fm)

-- | Change the current file.
openFile :: Handle -> FilePath -> IO ()
openFile fm filename = do
  fileContents <- TIO.readFile filename
  writeIORef (_fmFileName fm) filename
  writeIORef (_fmFileContents fm) fileContents
  writeIORef (_fmFileHash fm) (hashContents fileContents)

-- | Return whether the file has changed since the last save.
hasFileChanged :: Handle -> IO Bool
hasFileChanged fm = do
  fileContents <- readIORef $ _fmFileContents fm
  lastFileContentsHash <- readIORef $ _fmFileHash fm
  let fileContentsHash = hashContents fileContents
  pure $ fileContentsHash /= lastFileContentsHash

-- | Save the current file contents to disk and update the file hash.
save :: Handle -> IO ()
save fm = do
  fileName <- readIORef $ _fmFileName fm
  fileContents <- readIORef $ _fmFileContents fm
  writeIORef  (_fmFileHash fm) (hashContents fileContents)
  TIO.writeFile fileName fileContents

hashContents :: Text -> Digest SHA1
hashContents =  hash . TE.encodeUtf8
