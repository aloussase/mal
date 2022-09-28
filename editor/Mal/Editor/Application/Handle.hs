module Mal.Editor.Application.Handle
(
    Handle
  , new
  , setFileName
  , hasUnsavedChanges
  , appFileName
  , appTextEditor
)
where

import           Mal.Editor.TextEditor (TextEditor)
import qualified Mal.Editor.TextEditor as TextEditor

import           Control.Lens
import           Crypto.Hash
import           Data.IORef
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

data Handle =
  Handle
  { _appFileName   :: IORef (Maybe FilePath)      -- ^ The currently open file.
  , _appTextEditor :: TextEditor                  -- ^ The text editor.
  , appFileHash    :: IORef (Maybe String)    -- ^ Hash of the last saved file.
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

-- | Returns whether the editor has any unsaved changes.
hasUnsavedChanges :: Handle -> IO Bool
hasUnsavedChanges handle = do
  fileHash <- readIORef (appFileHash handle)
  editorContents <- TextEditor.getContents (handle ^. appTextEditor)

  pure $ case fileHash of
    Just fileHash' -> fileHash' == show (hash $ TE.encodeUtf8 editorContents :: Digest SHA1)
    Nothing -> not (T.null editorContents)


