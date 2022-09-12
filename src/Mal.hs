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
    , module Mal.PrettyPrinter
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
import           Mal.PrettyPrinter
import           Mal.Types

import           Control.Monad            (void)
import           Data.IORef               (IORef, newIORef)
import           System.IO                (readFile')

-- | 'emptyScope' returns a new empty 'MalScope' wrapped in an @IORef@.
emptyScope :: IO (IORef MalScope)
emptyScope =  newIORef Env.empty

coreFile :: FilePath
coreFile = "mal/core.mal"

-- | Execute the provided Mal program, using @scope@ as the initial interpreter state.
run :: Maybe MalFilename -> IORef MalScope -> String -> IO MalType
run filename initialScope ast = do
    -- FIXME: Use Text for parsing instead of String.
    core <- readFile' coreFile
    void $ eval (Just (MkMalFilename coreFile)) initialScope (parse (Just $ MkMalFilename coreFile) core)
    eval filename initialScope (parse filename ast)
