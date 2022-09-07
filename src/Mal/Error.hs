module Mal.Error where

newtype MalError = ParseError String
    deriving (Eq)

instance Show MalError where
    show (ParseError s) = s
