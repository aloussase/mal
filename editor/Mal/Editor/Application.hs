module Mal.Editor.Application
(
  run
)
where

import qualified Mal.Editor.Actions            as Actions
import qualified Mal.Editor.Application.Handle as App
import qualified Mal.Editor.ExecutionWindow    as ExecutionWindow
import qualified Mal.Editor.MenuBar            as MenuBar
import qualified Mal.Editor.TextEditor         as TextEditor
import qualified Mal.Editor.Toolbar            as ToolBar

import           Control.Monad                 (void)
import           Data.Text                     (Text)
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk

appId :: Text
appId = "io.github.aloussase.mal"

run :: IO ()
run =  do
  app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (runApplication app)
  _ <- Gio.applicationRun app Nothing
  return ()

runApplication :: Gtk.Application -> IO ()
runApplication app = do
  win <- Gtk.applicationWindowNew app

  Gtk.setWindowTitle win "Mal Editor"
  Gtk.setWindowDefaultWidth win 800
  Gtk.setWindowDefaultHeight win 600

  (infoBar, infoLabel) <- createInfoBar
  (textEditor, panedWidget, executionOutputTextEditor) <- createMainArea

  appState <- App.new app textEditor infoBar infoLabel executionOutputTextEditor
  Actions.createActions appState

  -- Create the toolbars
  toolbar <- ToolBar.new
  menuBar <- MenuBar.new

  -- Create the main layout
  mainLayout <- Gtk.boxNew Gtk.OrientationVertical 0

  Gtk.boxAppend mainLayout menuBar
  Gtk.boxAppend mainLayout toolbar
  Gtk.boxAppend mainLayout infoBar
  Gtk.boxAppend mainLayout panedWidget

  Gtk.windowSetChild win $ Just mainLayout
  void $ Gtk.widgetGrabFocus textEditor

  Gtk.widgetShow win

createMainArea :: IO (Gtk.TextView, Gtk.Paned, Gtk.TextView)
createMainArea = do
  textEditor <- TextEditor.empty
  editorWindow <- Gtk.scrolledWindowNew
  Gtk.scrolledWindowSetChild editorWindow $ Just textEditor
  (executionOutputTextEditor, executionWindow) <- ExecutionWindow.new
  panedWidget <- Gtk.panedNew Gtk.OrientationVertical
  Gtk.panedSetStartChild panedWidget $ Just editorWindow
  Gtk.panedSetEndChild panedWidget $ Just executionWindow
  pure (textEditor, panedWidget, executionOutputTextEditor)

createInfoBar :: IO (Gtk.InfoBar, Gtk.Label)
createInfoBar = do
  infoBar <- Gtk.infoBarNew
  Gtk.infoBarSetMessageType infoBar Gtk.MessageTypeInfo
  Gtk.widgetSetValign infoBar Gtk.AlignCenter
  Gtk.widgetHide infoBar
  infoLabel <- Gtk.labelNew Nothing
  Gtk.infoBarAddChild infoBar infoLabel
  pure (infoBar, infoLabel)


