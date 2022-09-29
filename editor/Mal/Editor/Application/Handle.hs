{-# LANGUAGE TypeApplications #-}
module Mal.Editor.Application.Handle
(
    Handle
  , new
  , reset
  , setFile
  , hasUnsavedChanges
  , appFileName
  , appTextEditor
  , appApplication
)
where

import           Mal.Editor.TextEditor (TextEditor)
import qualified Mal.Editor.TextEditor as TextEditor

import           Control.Lens
import           Crypto.Hash
import           Data.IORef
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified GI.Gtk                as Gtk
import           System.IO             (readFile')

data Handle =
  Handle
  { _appApplication :: Gtk.Application
  , _appFileName    :: IORef (Maybe FilePath)      -- ^ The currently open file.
  , _appTextEditor  :: TextEditor                  -- ^ The text editor.
  , _appFileHash    :: IORef (Maybe String)    -- ^ Hash of the last saved file.
  }

makeLenses ''Handle

-- | Create a new application state.
new :: Gtk.Application -> TextEditor -> IO Handle
new application textEditor = do
  fileName <- newIORef Nothing
  fileHash <- newIORef Nothing
  pure $ Handle application fileName textEditor fileHash

-- | Reset the application to a blank slate.
reset :: Handle -> IO ()
reset handle = do
  writeIORef (handle ^. appFileName) Nothing
  writeIORef (handle ^. appFileHash) Nothing
  TextEditor.setContents (handle ^. appTextEditor) ""

-- | Set the application's current file.
setFile :: Handle -> FilePath -> IO ()
setFile handle fileName = do
  writeIORef (handle^.appFileName) $ Just fileName
  fileContents <- readFile' fileName
  TextEditor.setContents (handle^.appTextEditor) $ T.pack fileContents
  writeIORef (handle^.appFileHash) $ Just (hashString fileContents)

-- | Returns whether the editor has any unsaved changes.
hasUnsavedChanges :: Handle -> IO Bool
hasUnsavedChanges handle = do
  fileHash <- readIORef (handle ^. appFileHash)
  editorContents <- TextEditor.getContents (handle ^. appTextEditor)
  pure $ case fileHash of
    Just fileHash' -> fileHash' == hashString (T.unpack editorContents)
    Nothing        -> not (T.null editorContents)

hashString :: String -> String
hashString = show @(Digest SHA1) . hash . TE.encodeUtf8 . T.pack

