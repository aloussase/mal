{-# LANGUAGE TypeFamilies #-}
module Mal.Editor.Notification.Renderer where

import           Mal.Editor.InfoBar

import           Control.Lens
import           Data.Text          (Text)
import qualified GI.Gtk             as Gtk

class NotificationRenderer a where
  type Message a

  showNotification :: a -> Message a -> IO ()

  hideNotification :: a -> IO ()


instance NotificationRenderer InfoBar where
  type Message InfoBar = Text

  showNotification infoBar message = do
    Gtk.labelSetText (infoBar^.infoBarLabel) message
    Gtk.widgetShow $ infoBar^.infoBarBar

  hideNotification infoBar = Gtk.widgetHide $ infoBar^.infoBarBar
