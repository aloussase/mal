module Mal.Editor.MessageDialog
(
    OnConfirm
  , OnCancel
  , ofConfirmation
)
where

import           Control.Monad (void)
import           Data.Text     (Text)
import qualified GI.Gtk        as Gtk

type OnConfirm = IO ()
type OnCancel = IO ()

ofConfirmation :: Gtk.Window -> Text -> OnConfirm -> OnCancel -> IO ()
ofConfirmation parentWindow message onConfirm onCancel = do
  dialog <- Gtk.dialogNew

  okBtn <- Gtk.dialogAddButton dialog "Ok" $ fromIntegral $ fromEnum Gtk.ResponseTypeOk
  cancelBtn <- Gtk.dialogAddButton dialog "Cancel" $ fromIntegral $ fromEnum Gtk.ResponseTypeCancel

  Gtk.widgetSetMarginEnd okBtn 5
  Gtk.widgetSetMarginBottom okBtn 10
  Gtk.widgetSetMarginEnd cancelBtn 5
  Gtk.widgetSetMarginBottom cancelBtn 10

  lbl <- Gtk.labelNew $ Just message
  Gtk.widgetSetVexpand lbl True

  contentArea <- Gtk.dialogGetContentArea dialog
  Gtk.widgetSetValign contentArea Gtk.AlignCenter
  Gtk.widgetSetHalign contentArea Gtk.AlignCenter
  Gtk.boxAppend contentArea lbl

  void $ Gtk.onDialogResponse dialog $ \responseId -> do
    case toEnum $ fromIntegral responseId of
      Gtk.ResponseTypeOk     -> onConfirm
      Gtk.ResponseTypeCancel -> onCancel
      _                      -> pure ()

    Gtk.windowClose dialog

  Gtk.windowSetDefaultSize dialog 350 200
  Gtk.windowSetTransientFor dialog $ Just parentWindow
  Gtk.windowSetModal dialog True
  Gtk.widgetShow dialog
