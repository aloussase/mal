module Mal (
      module Mal.Internal.Types
    , module Mal.Internal.Parser
    , module Mal.Internal.Interpreter
    , module Mal.Error
    , MalResult
    , run
) where

import           Mal.Error
import           Mal.Internal.Interpreter
import           Mal.Internal.Parser
import           Mal.Internal.Types

type MalResult = Either MalError MalType

run :: String -> IO MalResult
run source = do
    let parseResult = parse source
    case parseResult of
        Left err     -> pure $ Left err
        Right result -> eval result
