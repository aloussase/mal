module Mal (
      module Mal.Internal.Types
    , module Mal.Internal.Parser
    , module Mal.Internal.Interpreter
    , module Mal.Error
    , run
) where

import           Mal.Error
import           Mal.Internal.Interpreter
import           Mal.Internal.Parser
import           Mal.Internal.Types

run :: String -> IO MalType
run = eval . parse
