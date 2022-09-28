module Main where

import qualified Mal.Editor.Gui.MenuBar    as MenuBar
import qualified Mal.Editor.Gui.TextEditor as TextEditor
import qualified Mal.Editor.Gui.Toolbar    as Toolbar

import qualified Mal

import           Control.Monad             (forM_, void)
import           Data.Text                 (Text)
import qualified GI.Gio                    as Gio
import qualified GI.Gtk                    as Gtk

appId :: Text
appId = "io.github.aloussase.mal"

main :: IO ()
main = do
  app <- Gtk.applicationNew (Just appId) []
  _ <- Gio.onApplicationActivate app (run app)
  _ <- Gio.applicationRun app Nothing
  return ()

run :: Gtk.Application -> IO ()
run app = do
  win <- Gtk.applicationWindowNew app
  Gtk.setWindowTitle win "Mal Editor"
  Gtk.setWindowDefaultWidth win 800
  Gtk.setWindowDefaultHeight win 600

  layout <- Gtk.boxNew Gtk.OrientationVertical 10

  textEditor <- TextEditor.empty
  toolbar <- createToolBar textEditor
  menuBar <- MenuBar.new

  Gtk.boxAppend layout menuBar
  Gtk.boxAppend layout toolbar
  Gtk.boxAppend layout textEditor

  Gtk.windowSetChild win $ Just layout

  Gtk.widgetShow textEditor
  void $ Gtk.widgetGrabFocus textEditor

  void $ createApplicationActions app textEditor
  Gtk.widgetShow win

createApplicationActions :: Gtk.Application -> TextEditor.TextEditor -> IO ()
createApplicationActions app textEditor = do
  quitAction <- Gio.simpleActionNew "quit" Nothing
  void $ Gio.onSimpleActionActivate quitAction (const $ Gio.applicationQuit app)

  runCodeAction <- Gio.simpleActionNew "run-code" Nothing
  void $ Gio.onSimpleActionActivate runCodeAction (const $ runMal textEditor)

  forM_ [quitAction , runCodeAction] (Gio.actionMapAddAction app)

createToolBar :: TextEditor.TextEditor -> IO Toolbar.ToolBar
createToolBar textEditor = Toolbar.new (runMal textEditor)

-- TODO: pass in a filename
-- TODO: handle exceptions
runMal :: TextEditor.TextEditor -> IO ()
runMal textEditor = do
  program <- TextEditor.getContents textEditor
  result <- Mal.runOnce Nothing program
  print result
