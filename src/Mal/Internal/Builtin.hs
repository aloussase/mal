module Mal.Internal.Builtin where

import           Mal.Error
import           Mal.Internal.Types

import           Control.Exception          (throw)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.List                  (foldl')

type Function = [MalType] -> ReaderT MalEnv IO MalType

reduceMalNumbers :: String -> Int -> (Int -> Int -> Int) -> [MalType] -> ReaderT MalEnv IO MalType
reduceMalNumbers funcName init f xs = pure $ mkMalNumber (foldl' go init xs)
    where
        go acc (MalAtom (MalNumber x)) = f acc x
        go _ x                         = throw $ InvalidArgs funcName [x]

plus :: Function
plus = reduceMalNumbers "+" 0 (+)

sub :: Function
sub = reduceMalNumbers "-" 0 (-)

quot :: Function
quot xs@(MalAtom (MalNumber x):_) = reduceMalNumbers "/" (x^x) div xs
quot (x:_)                        = throw $ InvalidArgs "/" [x]
quot _                            = throw $ ExpectedArgs "/"

mult :: Function
mult = reduceMalNumbers "*" 1 (*)
