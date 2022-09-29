{-# LANGUAGE TypeApplications #-}
module Mal.Editor.Application.Handle
(
    Handle
  , new
  , reset
  , setFile
  , getFileName
  , notify
  , hasUnsavedChanges
  , appFileName
  , appTextEditor
  , appApplication
)
where

import           Mal.Editor.TextEditor (TextEditor)
import qualified Mal.Editor.TextEditor as TextEditor

import           Control.Concurrent    (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad         (void)
import           Crypto.Hash
import           Data.IORef
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified GI.GLib               as GLib
import qualified GI.Gtk                as Gtk
import           System.IO             (readFile')

data Handle =
  Handle
  { _appApplication :: Gtk.Application
  , _appFileName    :: IORef (Maybe FilePath)      -- ^ The currently open file.
  , _appTextEditor  :: TextEditor                  -- ^ The text editor.
  , _appFileHash    :: IORef (Maybe String)        -- ^ Hash of the last saved file.
  , _appInfoBar     :: Gtk.InfoBar                 -- ^ For notifications.
  , _appInfoLabel   :: Gtk.Label                   -- ^ The inner label of the info bar.
  }

makeLenses ''Handle

-- | Create a new application state.
new ::
  Gtk.Application
  -> TextEditor
  -> Gtk.InfoBar
  -> Gtk.Label
  -> IO Handle
new application textEditor infoBar infoLabel = do
  -- Start with no file.
  fileName <- newIORef Nothing
  fileHash <- newIORef Nothing
  pure $ Handle application fileName textEditor fileHash infoBar infoLabel

-- | Reset the application to a blank slate.
reset :: Handle -> IO ()
reset handle = do
  writeIORef (handle ^. appFileName) Nothing
  writeIORef (handle ^. appFileHash) Nothing
  TextEditor.setContents (handle ^. appTextEditor) ""

-- | Get the application's current file name.
getFileName :: Handle -> IO (Maybe FilePath)
getFileName = readIORef . view appFileName

-- | Set the application's current file.
setFile :: Handle -> FilePath -> IO ()
setFile handle fileName = do
  writeIORef (handle^.appFileName) $ Just fileName
  readFile' fileName >>= \fileContents -> do
    writeIORef (handle^.appFileHash) $ Just (hashString fileContents)
    TextEditor.setContents (handle^.appTextEditor) $ T.pack fileContents

-- | Returns whether the editor has any unsaved changes.
hasUnsavedChanges :: Handle -> IO Bool
hasUnsavedChanges handle = do
  fileHash <- readIORef (handle^.appFileHash)
  editorHash <- hashString . T.unpack <$> TextEditor.getContents (handle^.appTextEditor)
  case fileHash of
    Just fileHash' -> pure $ fileHash' /= editorHash
    Nothing        -> pure $ not . null $ editorHash


notify :: Handle -> Text -> IO ()
notify handle message = do
  Gtk.labelSetText (handle^.appInfoLabel) message
  _ <- forkIO $ do
        threadDelay (10^6)
        void $ GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
                Gtk.widgetHide (handle^.appInfoBar)
                pure False
  Gtk.widgetShow $ handle^.appInfoBar

hashString :: String -> String
hashString = show @(Digest SHA1) . hash . TE.encodeUtf8 . T.pack

