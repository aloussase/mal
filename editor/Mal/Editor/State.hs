module Mal.Editor.State where

import           Data.Text    (Text)
import qualified Data.Text.IO as TIO

data EditorState =
  MkEditorState
  { contents :: !Text
  , fileName :: Maybe FilePath
  }
  deriving (Show)

-- | Create a new empty editor state.
new :: EditorState
new = MkEditorState "" Nothing

-- | Create a new EditorState with the contents of the given file name.
ofFile :: FilePath -> IO EditorState
ofFile fileName = MkEditorState <$> TIO.readFile fileName <*> pure (Just fileName)
