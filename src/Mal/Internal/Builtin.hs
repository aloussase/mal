module Mal.Internal.Builtin where

import           Mal.Error
import           Mal.Internal.Types

import           Control.Exception  (throw)
import           Data.List          (foldl')

type Function = MalScope -> [MalType] -> MalType

reduceMalNumbers :: String -> Int -> (Int -> Int -> Int) -> [MalType] -> MalType
reduceMalNumbers funcName init f xs = mkMalNumber (foldl' go init xs)
    where
        go acc (MalAtom (MalNumber x)) = f acc x
        go _ x                         = throw $ InvalidArgs funcName [x]

plus :: Function
plus _ = reduceMalNumbers "+" 0 (+)

sub :: Function
sub _ = reduceMalNumbers "-" 0 (-)

quot :: Function
quot _ xs@(MalAtom (MalNumber x):_) = reduceMalNumbers "/" (x^x) div xs
quot _ (x:_)                        = throw $ InvalidArgs "/" [x]
quot _ _                            = throw $ ExpectedArgs "/"

mult :: Function
mult _ = reduceMalNumbers "*" 1 (*)
