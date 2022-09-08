module Mal (
      module Mal.Types
    , module Mal.Error
    , module Mal.Internal.Interpreter
    , module Mal.Internal.Parser
    , emptyScope
    , run
) where

import           Mal.Error
import qualified Mal.Internal.Environment as Env
import           Mal.Internal.Interpreter
import           Mal.Internal.Parser
import           Mal.Types

import           Data.IORef               (IORef, newIORef)

emptyScope :: IO (IORef MalScope)
emptyScope =  newIORef Env.empty

run :: IORef MalScope -> String -> IO MalType
run scope = eval scope . parse
