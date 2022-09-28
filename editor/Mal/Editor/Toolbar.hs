module Mal.Editor.Toolbar
(
    new
  , ToolBar
  , ToolBarButton
)
where

import           Mal.Editor.Actions
import qualified Mal.Editor.Application.Handle as App

import           Data.Text                     (Text)
import qualified GI.Gtk                        as Gtk

type ToolBar = Gtk.Box
type ToolBarButton = Gtk.Button

new :: App.Handle -> IO ToolBar
new _ = do
  toolbar <- Gtk.boxNew Gtk.OrientationHorizontal 0

  Gtk.widgetSetValign toolbar Gtk.AlignCenter

  runButton <- createToolBarButton "media-playback-start"
  Gtk.actionableSetActionName runButton (Just $ toActionName AppRunCode)

  Gtk.boxAppend toolbar runButton

  pure toolbar


createToolBarButton :: Text -> IO ToolBarButton
createToolBarButton iconName = do
  button <- Gtk.buttonNewFromIconName (Just iconName)
  Gtk.widgetSetMarginTop button 5
  Gtk.widgetSetMarginStart button 5
  pure button
