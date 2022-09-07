module Mal.Internal.Environment where

import           Mal.Error
import           Mal.Internal.Types

import           Control.Applicative     ((<|>))
import           Data.Either.Combinators (maybeToRight)
import           Data.Map                (Map, (!?))
import qualified Data.Map                as M

data MalScope = MkMalScope
    { parent   :: Maybe MalScope
    , bindings :: Map String MalType
    }

type MalFunction = MalScope -> [MalType] -> Either MalError MalType

empty :: MalScope
empty = MkMalScope Nothing M.empty

find :: MalScope -> String -> Either MalError MalType
find scope s = maybeToRight (UnboundSymbol s) (go (parent scope) (bindings scope))
    where
        go (Just scope') bindings' = bindings' !? s <|> go (parent scope') (bindings scope')
        go _ bindings'             = bindings' !? s
