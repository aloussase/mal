module Mal.Editor.Actions
(
    AppAction (..)
  , createActions
  , getAction
  , toActionName
)
where

import qualified Mal

import qualified Mal.Editor.TextEditor as TextEditor
import           Mal.Editor.Types

import           Control.Lens
import           Control.Monad         (void)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified GI.Gio                as Gio

data AppAction = AppQuit | AppRunCode

createActions :: ApplicationState -> IO ()
createActions appState = do
  Just app <- Gio.applicationGetDefault

  quitAction <- Gio.simpleActionNew "quit" Nothing
  void $ Gio.onSimpleActionActivate quitAction (const $ getAction AppQuit appState)

  runCodeAction <- Gio.simpleActionNew "run-code" Nothing
  void $ Gio.onSimpleActionActivate runCodeAction (const $ getAction AppRunCode appState)

  mapM_  (Gio.actionMapAddAction app)
    [ quitAction
    , runCodeAction
    ]

toActionName :: AppAction -> Text
toActionName AppQuit    = "app.quit"
toActionName AppRunCode = "app.run-code"

instance Show AppAction where show = T.unpack . toActionName

getAction :: AppAction -> ApplicationState -> IO ()
getAction AppRunCode appState = do
  program <- TextEditor.getContents $ appState^.appTextEditor
  result <- Mal.runOnce Nothing program
  print result
getAction AppQuit _ = do
  Just app <- Gio.applicationGetDefault
  Gio.applicationQuit app


