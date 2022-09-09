{-| Defines errors that can occur during a Mal program execution. -}
module Mal.Error where

import           Mal.Types

import           Control.Exception

-- | The different errors that can occur during a Mal program execution.
data MalError =
        ParseError String
        | UnboundSymbol String
        | InvalidArgs String [MalType]
        | ExpectedArgs String
        | NotAFunction MalType
        | InvalidSignature String
    deriving (Eq)

instance Exception MalError

instance Show MalError where
    show (ParseError s)    = s
    show (UnboundSymbol s) = "unbound symbol: " <> s
    show (InvalidArgs f args) = "invalid arguments for function " <> f <> ": " <> show args
    show (NotAFunction t) = "tried to call non-function " <> show t
    show (ExpectedArgs s) = "invalid arguments for function " <> s <> ", expected at least 1"
    show (InvalidSignature s) = "invalid function signature: " <> s
