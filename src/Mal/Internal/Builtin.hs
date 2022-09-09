{-|
 The builtin functions of the Mal programming language.
 -}
module Mal.Internal.Builtin (
    -- * Arithmetic functions
      plus
    , sub
    , mult
    , quot
    -- * Logic functions
    , eq
    , lessThan
    , lessThanEq
    , greaterThan
    , greaterThanEq
     -- * IO functions
    , prn
    -- * List functions
    , list
    , isList
    , isEmpty
    , count
) where

import           Mal.Class
import           Mal.Error
import           Mal.Types

import           Prelude                    hiding (quot)

import           Control.Exception          (throw, throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.List                  (foldl1')


type Function = [MalType] -> ReaderT MalEnv IO MalType

-- Arithmetic functions
reduceMalNumbers :: String -> (Int -> Int -> Int) -> [MalType] -> ReaderT MalEnv IO MalType
reduceMalNumbers funcName f xs = pure $ foldl1' go xs
  where
    go (MalAtom (MalNumber acc)) (MalAtom (MalNumber x)) = mkMalNumber $ f acc x
    go _ x                         = throw $ InvalidArgs funcName [x]

-- | 'plus' returns the sum of its arguments.
plus :: Function
plus = reduceMalNumbers "+" (+)

-- | 'sub' returns the subtraction of its arguments.
--
-- >>> (- 1 2 3)
-- -4
sub :: Function
sub = reduceMalNumbers "-" (-)

-- | 'quot' returns the division of its arguments.
--
-- >>> (/ 8 4 2)
-- 1
quot :: Function
quot = reduceMalNumbers "/" div

-- | 'mult' returns the multiplication of its arguments.
--
-- >>> (* 1 2 3)
-- 6
mult :: Function
mult = reduceMalNumbers "*" (*)

-- List functions

-- | Return a list consisting of the provided elements.
--
-- >>> (list 1 2 3)
-- (1 2 3)
list :: Function
list = liftMalType

-- | Return True if the argument is a list.
--
-- >>> (list? (list 1 2 3))
-- #t
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

-- Logic functions

compareNumbers :: String -> (Int -> Int -> Bool) -> Function
compareNumbers _ cmp [MalAtom (MalNumber x), MalAtom (MalNumber y)] = liftMalType $ cmp x y
compareNumbers funcName _ xs  = liftIO $ throwIO (InvalidArgs funcName xs)

-- | 'eq' returns true if the provided arguments are equal.
eq :: Function
eq = compareNumbers "=" (==)

-- | 'lessThan' return true if the first argument is less than the second one.
--
-- >>> (< 1 2)
-- #t
lessThan :: Function
lessThan = compareNumbers "<" (<)

-- | 'lessThanEq' return true if the first argument is less than or equal to the
-- second one.
lessThanEq :: Function
lessThanEq = compareNumbers "<=" (<=)

-- | 'greaterThan' return true if the first argument is greater than the second one.
greaterThan :: Function
greaterThan = compareNumbers ">" (>)

-- | 'greaterThanEq' return true if the first argument is greater than or equal to the
-- second one.
greaterThanEq :: Function
greaterThanEq = compareNumbers ">=" (>=)

-- IO Functions

-- | 'prn' prints the provided argument.
prn :: Function
prn [o] = do
    liftIO $ print o
    pure mkMalNil
prn xs = liftIO $ throwIO (InvalidArgs "prn" xs)
