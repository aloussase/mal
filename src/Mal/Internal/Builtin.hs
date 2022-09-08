module Mal.Internal.Builtin where

import           Mal.Class
import           Mal.Error
import           Mal.Types

import           Control.Exception          (throw, throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.List                  (foldl')

type Function = [MalType] -> ReaderT MalEnv IO MalType

-- | Arithmetic functions

reduceMalNumbers :: String -> Int -> (Int -> Int -> Int) -> [MalType] -> ReaderT MalEnv IO MalType
reduceMalNumbers funcName init f xs = liftMalType (foldl' go init xs)
    where
        go acc (MalAtom (MalNumber x)) = f acc x
        go _ x                         = throw $ InvalidArgs funcName [x]

plus :: Function
plus = reduceMalNumbers "+" 0 (+)

sub :: Function
sub = reduceMalNumbers "-" 0 (-)

quot :: Function
quot xs@(MalAtom (MalNumber x):_) = reduceMalNumbers "/" (x^x) div xs
quot (x:_)                        = liftIO $ throwIO $ InvalidArgs "/" [x]
quot _                            = liftIO $ throwIO $ ExpectedArgs "/"

mult :: Function
mult = reduceMalNumbers "*" 1 (*)

-- | List functions.

-- | Return a list consisting of the provided elements.
list :: Function
list = liftMalType

-- | Return True if the argument is a list.
isList :: Function
isList [MalList _] = liftMalType True
isList _           = liftMalType False

-- | Return true is the argument is a list and is empty, false otherwise.
isEmpty :: Function
isEmpty [MalList (MkMalList [])] = liftMalType True
isEmpty _                        = liftMalType False

-- | Return the number of elements in the provided list.
count :: Function
count [MalList (MkMalList xs)] = liftMalType . length $ xs
count xs                       = liftIO $ throwIO (InvalidArgs "count" xs)
