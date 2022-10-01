module Mal.Editor.Statusbar
(
    Statusbar
  , new
  , setFileName
  , getBar
)
where

import           Data.Text (Text)
import qualified GI.Gtk    as Gtk

data Statusbar =
  Statusbar
  { sbFileLabel :: Gtk.Label
  , sbStatusBar :: Gtk.Box
  }

new :: IO Statusbar
new = do
  statusBar <- Gtk.boxNew Gtk.OrientationHorizontal 5
  fileLabel <- Gtk.labelNew Nothing

  Gtk.boxAppend statusBar fileLabel

  pure $ Statusbar fileLabel statusBar

setFileName :: Statusbar -> Text -> IO ()
setFileName sb = Gtk.labelSetText (sbFileLabel sb)

getBar :: Statusbar -> Gtk.Box
getBar = sbStatusBar
