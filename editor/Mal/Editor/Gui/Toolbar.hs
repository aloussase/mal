module Mal.Editor.Gui.Toolbar
(
    new
  , ToolBar
  , ToolBarButton
)
where

import           Data.Text (Text)
import qualified GI.Gtk    as Gtk

type ToolBar = Gtk.Box
type ToolBarButton = Gtk.Button

new :: IO () -> IO ToolBar
new runAction = do
  toolbar <- Gtk.boxNew Gtk.OrientationHorizontal 0

  -- TODO: center buttons in toolbar
  Gtk.widgetSetValign toolbar Gtk.AlignCenter

  runButton <- createToolBarButton "media-playback-start" runAction

  Gtk.boxAppend toolbar runButton

  pure toolbar


createToolBarButton :: Text -> IO () -> IO ToolBarButton
createToolBarButton iconName clickAction = do
  button <- Gtk.buttonNewFromIconName (Just iconName)
  _ <- Gtk.onButtonClicked button clickAction

  Gtk.widgetSetMarginTop button 5
  Gtk.widgetSetMarginStart button 5

  pure button
