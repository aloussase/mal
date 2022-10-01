module Mal.Editor.Application.Handle
(
    Handle
  , appApplication
  , appExecutionOutput
  , appFileManager
  , appNotificationHandle
  , appTextEditor
  , appStatusbar
  , new
  , openFile
  , reset
)
where

import qualified Mal.Editor.FileManager         as FileManager
import           Mal.Editor.InfoBar             (InfoBar)
import qualified Mal.Editor.Notification.Handle as Notification
import           Mal.Editor.Statusbar           (Statusbar)
import qualified Mal.Editor.Statusbar           as Statusbar
import           Mal.Editor.TextEditor          (TextEditor)
import qualified Mal.Editor.TextEditor          as TextEditor

import           Control.Lens
import           Control.Monad                  (void)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified GI.Gio                         as Gio
import qualified GI.Gtk                         as Gtk

data Handle =
  Handle
  { _appApplication        :: Gtk.Application
  , _appFileManager        :: FileManager.Handle
  , _appTextEditor         :: TextEditor
  , _appExecutionOutput    :: TextEditor
  , _appNotificationHandle :: Notification.Handle InfoBar Text
  , _appStatusbar          :: Statusbar
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

  statusbar <- Statusbar.new
  currentFileName <- FileManager.getFileName fileManager
  Statusbar.setFileName statusbar $ T.pack currentFileName

  pure $ Handle
    { _appApplication = application
    , _appTextEditor = textEditor
    , _appExecutionOutput = executionOutput
    , _appFileManager = fileManager
    , _appNotificationHandle = notificationHandle
    , _appStatusbar = statusbar
    }

-- | Reset the application to a blank slate.
reset :: Handle -> IO ()
reset handle = do
  FileManager.reset (handle^.appFileManager)

  newFileName <- FileManager.getFileName $ handle^.appFileManager
  newContents <- FileManager.getFileContents $ handle^.appFileManager

  Statusbar.setFileName (handle^.appStatusbar) $ T.pack newFileName

  TextEditor.setContents (handle^.appTextEditor) newContents

-- | Set the application's current file.
openFile :: Handle -> FilePath -> IO ()
openFile handle filename = do
  FileManager.openFile (handle^.appFileManager) filename

  fileContents <- FileManager.getFileContents (handle^.appFileManager)
  TextEditor.setContents (handle^.appTextEditor) fileContents

  Statusbar.setFileName (handle^.appStatusbar) $ T.pack filename
