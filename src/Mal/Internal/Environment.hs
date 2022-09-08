module Mal.Internal.Environment where

import           Mal.Error
import           Mal.Internal.Types

import           Control.Applicative     ((<|>))
import           Control.Exception       (throw)
import           Data.Either.Combinators (maybeToRight)
import           Data.Map                (Map, (!?))
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)

empty :: MalScope
empty = MkMalScope Nothing M.empty

fromList :: [(String, MalType)] -> MalScope
fromList xs = MkMalScope { parent = Nothing, bindings = M.fromList xs }

find :: MalScope -> String -> MalType
find scope s = fromMaybe (throw $ UnboundSymbol s) (go (parent scope) (bindings scope))
    where
        go (Just !scope') !bindings' = bindings' !? s <|> go (parent scope') (bindings scope')
        go !_ !bindings'             = bindings' !? s

insert :: MalScope -> String -> MalType -> MalScope
insert self@(MkMalScope _ m) name thing = self { bindings = M.insert name thing m }
