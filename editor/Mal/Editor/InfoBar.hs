module Mal.Editor.InfoBar
(
    InfoBar
  , new
  , infoBarBar
  , infoBarLabel
)

where

import           Control.Lens
import qualified GI.Gtk       as Gtk

data InfoBar =
  InfoBar
  { _infoBarBar   :: Gtk.InfoBar
  , _infoBarLabel :: Gtk.Label
  }

makeLenses ''InfoBar

new :: IO InfoBar
new = do
  infoBar <- Gtk.infoBarNew
  Gtk.infoBarSetMessageType infoBar Gtk.MessageTypeInfo
  Gtk.widgetSetValign infoBar Gtk.AlignCenter
  Gtk.widgetHide infoBar

  infoLabel <- Gtk.labelNew Nothing
  Gtk.infoBarAddChild infoBar infoLabel

  pure $ InfoBar infoBar infoLabel

