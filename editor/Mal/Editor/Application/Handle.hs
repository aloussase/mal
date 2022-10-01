{-# LANGUAGE TypeApplications #-}
module Mal.Editor.Application.Handle
(
    Handle
  , appApplication
  , appExecutionOutput
  , appFileManager
  , appTextEditor
  , new
  , notify
  , reset
  , openFile
)
where

import qualified Mal.Editor.FileManager as FileManager
import           Mal.Editor.InfoBar     (InfoBar, infoBarBar, infoBarLabel)
import           Mal.Editor.TextEditor  (TextEditor)
import qualified Mal.Editor.TextEditor  as TextEditor

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Lens
import           Control.Monad          (void)
import           Data.Text              (Text)
import qualified GI.GLib                as GLib
import qualified GI.Gtk                 as Gtk

data Handle =
  Handle
  { _appApplication     :: Gtk.Application
  , _appFileManager     :: FileManager.Handle
  , _appTextEditor      :: TextEditor
  , _appInfoBar         :: InfoBar
  , _appExecutionOutput :: TextEditor
  }

makeLenses ''Handle

-- | Create a new application state.
new ::
  Gtk.Application
  -> TextEditor
  -> InfoBar
  -> TextEditor
  -> IO Handle
new application textEditor infoBar executionOutput = do
  fileManager <- FileManager.new Nothing
  pure $ Handle
    { _appApplication = application
    , _appTextEditor = textEditor
    , _appInfoBar = infoBar
    , _appExecutionOutput = executionOutput
    , _appFileManager = fileManager
    }

-- | Reset the application to a blank slate.
reset :: Handle -> IO ()
reset handle = do
  FileManager.reset (handle^.appFileManager)
  TextEditor.setContents (handle^.appTextEditor) ""

-- | Set the application's current file.
openFile :: Handle -> FilePath -> IO ()
openFile handle filename = do
  FileManager.openFile (handle^.appFileManager) filename
  fileContents <- FileManager.getFileContents (handle^.appFileManager)
  TextEditor.setContents (handle^.appTextEditor) fileContents

notify :: Handle -> Text -> IO ()
notify handle message = do
  Gtk.labelSetText (handle^.appInfoBar.infoBarLabel) message
  Gtk.widgetShow $ handle^.appInfoBar.infoBarBar

  void $ forkIO $ do
    threadDelay $ 10^6
    void $ GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $
            Gtk.widgetHide (handle^.appInfoBar.infoBarBar) >> pure False
