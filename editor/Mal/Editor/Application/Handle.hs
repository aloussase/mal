module Mal.Editor.Application.Handle
(
    Handle
  , appApplication
  , appExecutionOutput
  , appFileManager
  , appNotificationHandle
  , appTextEditor
  , new
  , openFile
  , reset
)
where

import qualified Mal.Editor.FileManager         as FileManager
import           Mal.Editor.InfoBar             (InfoBar)
import qualified Mal.Editor.Notification.Handle as Notification
import           Mal.Editor.TextEditor          (TextEditor)
import qualified Mal.Editor.TextEditor          as TextEditor

import           Control.Lens
import           Control.Monad                  (void)
import           Data.Text                      (Text)
import qualified GI.Gio                         as Gio
import qualified GI.Gtk                         as Gtk

data Handle =
  Handle
  { _appApplication        :: Gtk.Application
  , _appFileManager        :: FileManager.Handle
  , _appTextEditor         :: TextEditor
  , _appExecutionOutput    :: TextEditor
  , _appNotificationHandle :: Notification.Handle InfoBar Text
  }

makeLenses ''Handle

-- | Create a new application state.
new ::
  Gtk.Application
  -> TextEditor
  -> TextEditor
  -> Notification.Handle InfoBar Text
  -> IO Handle
new application textEditor executionOutput notificationHandle = do
  fileManager <- FileManager.new Nothing

  Notification.start notificationHandle
  void $ Gio.onApplicationShutdown application (Notification.close notificationHandle)

  pure $ Handle
    { _appApplication = application
    , _appTextEditor = textEditor
    , _appExecutionOutput = executionOutput
    , _appFileManager = fileManager
    , _appNotificationHandle = notificationHandle
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
