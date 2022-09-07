module Mal (
      module Mal.Internal.Types
    , module Mal.Internal.Parser
    , module Mal.Error
    , MalResult
) where

import           Mal.Error
import           Mal.Internal.Parser
import           Mal.Internal.Types

type MalResult = Either MalError MalType
