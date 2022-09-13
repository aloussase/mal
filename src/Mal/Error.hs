{-| Defines errors that can occur during a Mal program execution. -}
module Mal.Error where

import           Mal.Types

import           Control.Exception hiding (IndexOutOfBounds)

-- | The different errors that can occur during a Mal program execution.
data MalError =
        ParseError String                               -- ^ A parser error.
        | UnboundSymbol String                          -- ^ Signaled when the interpreter tries to resolve an unbound symbol.
        | InvalidArgs String [MalType] (Maybe String)   -- ^ Signaled when a function receives invalid arguments.
        | NotAFunction MalType                          -- ^ Signaled when the interpreter tries to call a non-functions.
        | InvalidSignature String                       -- ^ Signaled when a function is defined with an invalid function.
        | FileNotFound FilePath
        | IndexOutOfBounds Int Int                      -- ^ Signales when trying to access and out-of-bounds index of a list or vector.
    deriving (Eq)

instance Exception MalError

instance Show MalError where
    show (ParseError s) = s
    show (UnboundSymbol s) = "unbound symbol: " <> s

    show (FileNotFound filename) = "file not found: " <> filename

    show (InvalidArgs f args Nothing) = "invalid arguments for function " <> f <> ": " <> show args
    show (InvalidArgs f args (Just reason)) = mconcat [ "invalid arguments for function ", f, ": ", show args, "\n"
                                                      , reason
                                                      ]

    show (NotAFunction t) = "tried to call non-function " <> show t
    show (InvalidSignature s) = "invalid function signature: " <> s
    show (IndexOutOfBounds idx size) = "index " <> show idx <> " out of bounds for size " <> show size
