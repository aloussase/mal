module Mal.Error where

import           Mal.Internal.Types

data MalError =
        ParseError String
        | UnboundSymbol String
        | InvalidArgs String [MalType]
        | NotAFunction MalType
    deriving (Eq)

instance Show MalError where
    show (ParseError s)    = s
    show (UnboundSymbol s) = "unbound symbol: " <> s
    show (InvalidArgs f args) = "invalid arguments for function " <> f <> show args
    show (NotAFunction t) = "tried to call non-function " <> show t
