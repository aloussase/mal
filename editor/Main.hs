module Main where

import qualified Mal.Editor.Gui.TextEditor as TextEditor
import qualified Mal.Editor.Gui.Toolbar    as Toolbar

import qualified Mal

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

  Gtk.boxAppend layout toolbar
  Gtk.boxAppend layout textEditor

  Gtk.windowSetChild win $ Just layout

  Gtk.widgetShow textEditor
  _ <- Gtk.widgetGrabFocus textEditor

  Gtk.widgetShow win


createToolBar :: TextEditor.TextEditor -> IO Toolbar.ToolBar
createToolBar textEditor = Toolbar.new runAction
  where
    runAction = do
      program <- TextEditor.getContents textEditor
      result <- runMal program
      print result

-- TODO: pass in a filename
-- TODO: handle exceptions
runMal :: Text -> IO Text
runMal = do
  Mal.runOnce Nothing
