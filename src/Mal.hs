module Mal (
      module Mal.Internal.Types
    , module Mal.Internal.Parser
    , module Mal.Internal.Interpreter
    , module Mal.Error
    , run
    , emptyScope
) where

import           Mal.Error
import qualified Mal.Internal.Environment as Env
import           Mal.Internal.Interpreter
import           Mal.Internal.Parser
import           Mal.Internal.Types

import           Data.IORef               (IORef, newIORef)

emptyScope :: IO (IORef MalScope)
emptyScope =  newIORef Env.empty

run :: IORef MalScope -> String -> IO MalType
run scope = eval scope . parse
