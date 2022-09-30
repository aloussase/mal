{-# LANGUAGE ScopedTypeVariables #-}
module Mal.Editor.Actions
(
    AppAction (..)
  , createActions
  , getAction
  , toActionName
)
where

import qualified Mal

import           Mal.Editor.Application.Handle (appApplication, appTextEditor)
import qualified Mal.Editor.Application.Handle as App
import qualified Mal.Editor.MessageDialog      as MessageDialog
import qualified Mal.Editor.TextEditor         as TextEditor

import           Control.Lens
import           Control.Monad                 (forM_, void)
import           Data.GI.Base
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk

data AppAction =
  AppQuit
  | AppRunCode
  | AppNewFile
  | AppOpenFile
  | AppSaveFile
  | AppShowAbout


createAction :: App.Handle -> Text -> AppAction -> IO Gio.SimpleAction
createAction handle actionName actionType = do
  action <- Gio.simpleActionNew actionName Nothing
  void $ Gio.onSimpleActionActivate action (const $ getAction actionType handle)
  pure action

createActions :: App.Handle -> IO ()
createActions handle = do
  Just app <- Gio.applicationGetDefault

  actions <- sequence [ createAction handle "quit" AppQuit
                      , createAction handle "run-code" AppRunCode
                      , createAction handle "show-about" AppShowAbout
                      , createAction handle "new-file" AppNewFile
                      , createAction handle "open-file" AppOpenFile
                      , createAction handle "save-file" AppSaveFile
                      ]

  forM_ actions $ Gio.actionMapAddAction app

toActionName :: AppAction -> Text
toActionName AppQuit      = "app.quit"
toActionName AppRunCode   = "app.run-code"
toActionName AppNewFile   = "app.new-file"
toActionName AppOpenFile  = "app.open-file"
toActionName AppSaveFile  = "app.save-file"
toActionName AppShowAbout = "app.show-about"

instance Show AppAction where show = T.unpack . toActionName

getAction :: AppAction -> App.Handle -> IO ()

getAction AppRunCode handle = do
  program <- TextEditor.getContents $ handle ^. appTextEditor
  result <- Mal.runOnce Nothing program
  print result

getAction AppQuit _ = do
  Just app <- Gio.applicationGetDefault
  Gio.applicationQuit app

getAction AppNewFile handle = do
  Just activeWindow <- Gtk.applicationGetActiveWindow (handle ^. appApplication)
  hasUnsaved <- App.hasUnsavedChanges handle
  if not hasUnsaved then App.reset handle
  else
    MessageDialog.ofConfirmation
      activeWindow
      "There are unsaved changes, create a new file?"
      (App.reset handle)
      (pure ())

getAction AppOpenFile handle = do
  activeWindow <- Gtk.applicationGetActiveWindow (handle^.appApplication)

  fileChooserDialog :: Gtk.FileChooserDialog <- new Gtk.FileChooserDialog []

  Gtk.windowSetTitle fileChooserDialog $ Just "Open File"
  Gtk.fileChooserSetAction fileChooserDialog Gtk.FileChooserActionOpen

  _ <- Gtk.dialogAddButton fileChooserDialog "Open" $ fromIntegral (fromEnum Gtk.ResponseTypeOk)
  _ <- Gtk.dialogAddButton fileChooserDialog "Cancel" $ fromIntegral (fromEnum Gtk.ResponseTypeCancel)

  void $ Gtk.onDialogResponse fileChooserDialog $ \responseId -> do
    case toEnum $ fromIntegral responseId of
      Gtk.ResponseTypeOk -> do
          -- TODO: Should handle these errors.
          Just file <- Gtk.fileChooserGetFile fileChooserDialog
          Just fileName <- Gio.fileGetPath file
          App.setFile handle fileName
      _ -> pure ()
    Gtk.windowClose fileChooserDialog

  Gtk.windowSetTransientFor fileChooserDialog activeWindow
  Gtk.widgetShow fileChooserDialog

getAction AppSaveFile handle = do
  App.getFileName handle >>= maybe createAndSaveFile saveOpenFile
  where
    createAndSaveFile = do
      -- TODO
      print ("saving filename..." :: String)

    saveOpenFile fileName = do
      TextEditor.getContents (handle^.appTextEditor) >>= TIO.writeFile fileName
      App.notify handle "File saved"

getAction AppShowAbout _ = do
  aboutDialog <- Gtk.aboutDialogNew
  Gtk.aboutDialogSetAuthors aboutDialog ["Alexander Goussas"]
  Gtk.aboutDialogSetComments aboutDialog $ Just "Text editor for the Mal programming language."
  Gtk.aboutDialogSetCopyright aboutDialog $ Just "Alexander Goussas 2022"
  Gtk.aboutDialogSetProgramName aboutDialog $ Just "Mal Editor"
  Gtk.aboutDialogSetWebsite aboutDialog $ Just "https://github.com/aloussase/mal.git"
  Gtk.widgetShow aboutDialog
