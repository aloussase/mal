{-|
Contains definitions pertaining the interpreter environment.
 -}
module Mal.Internal.Environment (
      empty
    , fromList
    , find
    , insert
) where

import           Mal.Error
import           Mal.Types

import           Control.Applicative     ((<|>))
import           Control.Exception       (throw)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map                (Map, (!?))
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)

-- | 'empty' creates a new empty 'MalScope'.
empty :: MalScope
empty = MkMalScope Nothing M.empty

-- | 'fromList' creates a new 'MalScope' from the provided list.
fromList :: [(String, MalType)] -> MalScope
fromList xs = MkMalScope { parent = Nothing, bindings = M.fromList xs }

-- | 'find' looks up @s@ in @scope@ and returns its associated value.
-- If @s@ is not in @scope@, it is recursively searched for in the @scope@'s parent.
-- Finally, this function throws an 'UnboundSymbol' exception if @s@ is not bound.
find :: MalScope -> String -> MalType
find !scope !s = fromMaybe (throw $ UnboundSymbol s) (go (parent scope) (bindings scope))
    where
        go (Just !scope') !bindings' = bindings' !? s <|> go (parent scope') (bindings scope')
        go !_ !bindings'             = bindings' !? s

-- | 'insert' insertes a new key-value pair in the provided scope.
insert :: String -> MalType -> MalScope -> MalScope
insert name thing self@(MkMalScope _ m) = self { bindings = M.insert name thing m }
