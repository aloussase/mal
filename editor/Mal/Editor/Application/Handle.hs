module Mal.Editor.Application.Handle
(
    Handle
  , new
  , setFileName
  , appFileName
  , appTextEditor
)
where

import           Mal.Editor.TextEditor (TextEditor)

import           Control.Lens
import           Data.ByteString       (ByteString)
import           Data.IORef

data Handle =
  Handle
  { _appFileName   :: IORef (Maybe FilePath)      -- ^ The currently open file.
  , _appTextEditor :: TextEditor                  -- ^ The text editor.
  , appFileHash    :: IORef (Maybe ByteString)    -- ^ Hash of the last saved file.
  }

makeLenses ''Handle

-- | Create a new application state.
new :: TextEditor -> IO Handle
new textEditor = do
  fileName <- newIORef Nothing
  fileHash <- newIORef Nothing
  pure $ Handle fileName textEditor fileHash

-- | Set the application's file name
setFileName :: Handle -> FilePath -> IO ()
setFileName appState = writeIORef (appState^.appFileName) . Just
