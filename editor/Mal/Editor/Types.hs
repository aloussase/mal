module Mal.Editor.Types
(
    ApplicationState
  , appStateNew
  , appFileName
  , appTextEditor
)
where

import           Mal.Editor.TextEditor (TextEditor)

import           Control.Lens

data ApplicationState =
  ApplicationState
  { _appFileName   :: Maybe FilePath      -- ^ The currently open file
  , _appTextEditor :: TextEditor          -- ^ The open text editor
  }

makeLenses ''ApplicationState

-- | Create a new application state.
appStateNew :: TextEditor -> ApplicationState
appStateNew = ApplicationState Nothing
