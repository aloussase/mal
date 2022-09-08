module Mal.Error where

import           Mal.Internal.Types

import           Control.Exception

data MalError =
        ParseError String
        | UnboundSymbol String
        | InvalidArgs String [MalType]
        | ExpectedArgs String
        | NotAFunction MalType
    deriving (Eq)

instance Exception MalError

instance Show MalError where
    show (ParseError s)    = s
    show (UnboundSymbol s) = "unbound symbol: " <> s
    show (InvalidArgs f args) = "invalid arguments for function " <> f <> ": " <> show args
    show (NotAFunction t) = "tried to call non-function " <> show t
    show (ExpectedArgs s) = "invalid arguments for function " <> s <> ", expected at least 1"
