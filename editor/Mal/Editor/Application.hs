module Mal.Editor.Application
(
  run
)
where

import qualified Mal.Editor.Actions            as Actions
import qualified Mal.Editor.Application.Handle as App
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

  layout <- Gtk.boxNew Gtk.OrientationVertical 10

  textEditor <- TextEditor.empty
  appState <- App.new app textEditor

  scrolledWindow <- Gtk.scrolledWindowNew
  Gtk.scrolledWindowSetChild scrolledWindow $ Just textEditor

  toolbar <- ToolBar.new appState
  menuBar <- MenuBar.new

  Actions.createActions  appState

  Gtk.boxAppend layout menuBar
  Gtk.boxAppend layout toolbar
  Gtk.boxAppend layout scrolledWindow

  Gtk.windowSetChild win $ Just layout

  void $ Gtk.widgetGrabFocus textEditor
  Gtk.widgetShow win
