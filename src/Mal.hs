{-|
 The entry point to the library.

 A Mal program needs an initial state to be able to run. An empty one can be
 obtained with 'emptyScope'.

 @
    initialScope <- Mal.emptyScope
    result <- Mal.run initialScope "(prn \"Hello, World!\")"
 @

 This is necessary so that the interpreter state is not lost in REPL sessions.
 -}
module Mal (
      module Mal.Types
    , module Mal.Error
    , module Mal.Class
    , emptyScope
    , run
    , eval
    , parse
) where

import           Mal.Class
import           Mal.Error
import qualified Mal.Internal.Environment as Env
import           Mal.Internal.Interpreter (eval)
import           Mal.Internal.Parser      (parse)
import           Mal.Types

import           Data.IORef               (IORef, newIORef)

-- | 'emptyScope' returns a new empty 'MalScope' wrapped in an @IORef@.
emptyScope :: IO (IORef MalScope)
emptyScope =  newIORef Env.empty

-- | Execute the provided Mal program, using @scope@ as the initial interpreter state.
run :: IORef MalScope -> String -> IO MalType
run scope ast = do
    eval scope (parse "(def! not (fn* (a) (if a false true)))")
    eval scope (parse ast)
