{-|
Contains definitions pertaining the interpreter environment.
 -}
module Mal.Internal.Environment (
      empty
    , fromList
    , find
    , insert
    , getRoot
    , printScopes
    , withScope
) where

import           Mal.Error
import           Mal.Types

import           Control.Exception          (throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks)
import           Data.IORef                 (IORef, readIORef, writeIORef)
import           Data.Map                   (Map, (!?))
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust)

-- | 'empty' creates a new empty 'MalScope'.
empty :: MalScope
empty = MkMalScope Nothing M.empty

printScopes :: MalScope -> IO ()
printScopes (MkMalScope (Just parent) bindings) = do
    parentScope <- readIORef parent
    print bindings >> printScopes parentScope
printScopes (MkMalScope _ bindings) = print bindings

-- | 'fromList' creates a new 'MalScope' from the provided list.
fromList :: [(String, MalType)] -> MalScope
fromList xs = MkMalScope { scopeParent = Nothing, scopeBindings = M.fromList xs }

-- | 'find' looks up @s@ in @scope@ and returns its associated value.
-- If @s@ is not in @scope@, it is recursively searched for in the @scope@'s parent.
-- Finally, this function throws an 'UnboundSymbol' exception if @s@ is not bound.
find :: IORef MalScope -> String -> IO MalType
find !env !s = do
    currentScope <- readIORef env
    result <- go (scopeParent currentScope) (scopeBindings currentScope)
    maybe (throwIO $ UnboundSymbol s) pure result

    where
        go :: Maybe (IORef MalScope) -> Map String MalType -> IO (Maybe MalType)
        go Nothing currentScopeBindings = pure $ currentScopeBindings !? s
        go (Just currentScopeParent) currentScopeBindings
            | isJust (currentScopeBindings !? s) = pure $ currentScopeBindings !? s
            | otherwise = do
                parentScope <- readIORef currentScopeParent
                go (scopeParent parentScope) (scopeBindings parentScope)

-- | 'getRoot' returns the root environment reachable from the provided scope.
getRoot :: IORef MalScope -> IO (IORef MalScope)
getRoot aScope = do
    currentRef <- readIORef aScope
    case scopeParent currentRef of
        Just parent -> getRoot parent
        Nothing     -> pure aScope

-- | 'insert' insertes a new key-value pair in the provided scope.
insert :: String -> MalType -> MalScope -> MalScope
insert name thing self@(MkMalScope _ m) = self { scopeBindings = M.insert name thing m }

-- | Evaluate an interpreter action with a different scope.
withScope :: IORef MalScope -> Interpreter -> Interpreter
withScope newScopeRef action = do
  scopeRef <- asks interpreterScope
  oldScope <- liftIO $ readIORef scopeRef
  liftIO (readIORef newScopeRef >>= writeIORef scopeRef)
    *> action <* liftIO (writeIORef scopeRef oldScope)
