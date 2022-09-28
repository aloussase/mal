module Mal.Editor.Actions
(
    AppAction (..)
  , createActions
  , getAction
  , toActionName
)
where

import qualified Mal

import           Mal.Editor.Application.Handle (appTextEditor)
import qualified Mal.Editor.Application.Handle as App
import qualified Mal.Editor.TextEditor         as TextEditor

import           Control.Lens
import           Control.Monad                 (void)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified GI.Gio                        as Gio

data AppAction =
  AppQuit
  | AppRunCode
  | AppNewFile
  | AppOpenFile
  | AppSaveFile
  | AppShowAbout

createActions :: App.Handle -> IO ()
createActions handle = do
  Just app <- Gio.applicationGetDefault

  quitAction <- Gio.simpleActionNew "quit" Nothing
  void $ Gio.onSimpleActionActivate quitAction (const $ getAction AppQuit handle)

  runCodeAction <- Gio.simpleActionNew "run-code" Nothing
  void $ Gio.onSimpleActionActivate runCodeAction (const $ getAction AppRunCode handle)

  mapM_  (Gio.actionMapAddAction app)
    [ quitAction
    , runCodeAction
    ]

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

getAction AppNewFile _appState = undefined
getAction AppOpenFile _appState = undefined
getAction AppSaveFile _appState = undefined
getAction AppShowAbout _appState = undefined

