-- |
-- The builtin functions of the Mal programming language.
module Mal.Internal.Builtin
  ( builtins,

    -- * Arithmetic functions
    plus,
    sub,
    mult,
    quot,

    -- * Logic functions
    eq,
    lessThan,
    lessThanEq,
    greaterThan,
    greaterThanEq,

    -- * String functions
    prn,
    str,
    println,
    readString,

    -- * List functions
    list,
    isList,
    isEmpty,
    count,

    -- * IO functions
    slurp,

    -- * Atom functions
    atom,
    isAtom,
    deref,
    reset,
    swap,
  )
where

import           Mal.Class
import           Mal.Error
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Parser
import           Mal.PrettyPrinter
import           Mal.Types

import           Prelude                    hiding (quot)

import           Control.Concurrent.STM     (atomically, readTVarIO, swapTVar)
import           Control.Exception          (throw, throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Data.List                  (foldl', foldl1')
import qualified Data.Text                  as T
import           System.IO                  (readFile')

type BuiltinFunction = [MalType] -> ReaderT MalEnv IO MalType

builtins :: MalScope
builtins =
  Env.fromList $
    map
      (\(sym, fn) -> (sym, mkMalFunction sym fn))
      [ ("+", plus),
        ("-", sub),
        ("/", quot),
        ("*", mult),
        ("list", list),
        ("list?", isList),
        ("empty?", isEmpty),
        ("count", count),
        ("=", eq),
        ("<", lessThan),
        ("<=", lessThanEq),
        (">", greaterThan),
        (">=", greaterThanEq),
        ("prn", prn),
        ("str", str),
        ("println", println),
        ("read-string", readString),
        ("slurp", slurp),
        ("atom", atom),
        ("atom?", isAtom),
        ("deref", deref),
        ("reset!", reset),
        ("swap!", swap)
      ]

-- Arithmetic functions
reduceMalNumbers :: String -> (Int -> Int -> Int) -> [MalType] -> ReaderT MalEnv IO MalType
reduceMalNumbers funcName f xs = pure $ foldl1' go xs
  where
    go (MalAtom (MalNumber acc)) (MalAtom (MalNumber x)) = mkMalNumber $ f acc x
    go _ x = throw $ InvalidArgs funcName [x] Nothing

-- | 'plus' returns the sum of its arguments.
plus :: BuiltinFunction
plus = reduceMalNumbers "+" (+)

-- | 'sub' returns the subtraction of its arguments.
--
-- >>> (- 1 2 3)
-- -4
sub :: BuiltinFunction
sub = reduceMalNumbers "-" (-)

-- | 'quot' returns the division of its arguments.
--
-- >>> (/ 8 4 2)
-- 1
quot :: BuiltinFunction
quot = reduceMalNumbers "/" div

-- | 'mult' returns the multiplication of its arguments.
--
-- >>> (* 1 2 3)
-- 6
mult :: BuiltinFunction
mult = reduceMalNumbers "*" (*)

-- List functions

-- | Return a list consisting of the provided elements.
--
-- >>> (list 1 2 3)
-- (1 2 3)
list :: BuiltinFunction
list = liftMalType

-- | Return True if the argument is a list.
--
-- >>> (list? (list 1 2 3))
-- #t
isList :: BuiltinFunction
isList [MalList _] = liftMalType True
isList _           = liftMalType False

-- | Return true is the argument is a list and is empty, false otherwise.
isEmpty :: BuiltinFunction
isEmpty [MalList (MkMalList [])] = liftMalType True
isEmpty _                        = liftMalType False

-- | Return the number of elements in the provided list.
count :: BuiltinFunction
count [MalList (MkMalList xs)] = liftMalType . length $ xs
count xs = liftIO $ throwIO (InvalidArgs "count" xs (Just "expected a list"))

-- Logic functions

compareNumbers :: String -> (Int -> Int -> Bool) -> BuiltinFunction
compareNumbers _ cmp [MalAtom (MalNumber x), MalAtom (MalNumber y)] = liftMalType $ cmp x y
compareNumbers funcName _ xs = liftIO $ throwIO (InvalidArgs funcName xs (Just "expected two numbers"))

-- | 'eq' returns true if the provided arguments are equal.
eq :: BuiltinFunction
eq = compareNumbers "=" (==)

-- | 'lessThan' return true if the first argument is less than the second one.
--
-- >>> (< 1 2)
-- #t
lessThan :: BuiltinFunction
lessThan = compareNumbers "<" (<)

-- | 'lessThanEq' return true if the first argument is less than or equal to the
-- second one.
lessThanEq :: BuiltinFunction
lessThanEq = compareNumbers "<=" (<=)

-- | 'greaterThan' return true if the first argument is greater than the second one.
greaterThan :: BuiltinFunction
greaterThan = compareNumbers ">" (>)

-- | 'greaterThanEq' return true if the first argument is greater than or equal to the
-- second one.
greaterThanEq :: BuiltinFunction
greaterThanEq = compareNumbers ">=" (>=)

-- String Functions

-- | 'prn' prints the provided argument.
prn :: BuiltinFunction
prn xs = do
  liftIO . print . init . foldl' (\acc x -> acc <> T.unpack (showReadably False x <> " ")) "" $ xs
  pure mkMalNil

-- | 'println' prints the given arguments as formated by 'str'.
println :: BuiltinFunction
println xs = str xs >>= liftIO . print >> pure mkMalNil

-- | 'str' returns a string that is the result of joining the string the representation
-- of its arguments using a space character as a separator.
str :: BuiltinFunction
str =
  pure
    . mkMalString
    . init
    . foldl' (\acc x -> acc <> T.unpack (showReadably False x <> " ")) ""

-- | 'readString' parses the provided string into a @MalType@.
readString :: BuiltinFunction
readString [MalAtom (MalString form)] = do
    filename <- asks interpreterFilename
    pure . parse (Just filename) $ form
readString xs = throw $ InvalidArgs "read-string" xs (Just "expected a string")

-- IO functions

-- | 'slurp' takes a string as its only argument, treats it as a @FilePath@ and
-- returns the contents of the corresponding file.
slurp :: BuiltinFunction
slurp [MalAtom (MalString filename)] = do
  !contents <- liftIO $ readFile' filename
  pure $ mkMalString contents
slurp xs = throw $ InvalidArgs "slurp" xs (Just "expected a filename (string)")

-- Atom functions

-- | 'atom' creates a new atom.
atom :: BuiltinFunction
atom [t] = liftIO (mkMalAtom t)
atom xs  = liftIO $ throwIO (InvalidArgs "atom" xs (Just "expected a single argument"))

-- | 'isAtom' returns true if its argument is an atom.
isAtom :: BuiltinFunction
isAtom [MalAtomicCell _] = liftMalType True
isAtom _                 = liftMalType False

-- | 'deref' returns the value inside an atom.
deref :: BuiltinFunction
deref [MalAtomicCell (MkMalAtom ref)]= liftIO $ readTVarIO ref
deref xs = liftIO $ throwIO (InvalidArgs "deref" xs (Just "expected a single argument"))

-- | 'reset!' swaps the value inside an atom.
reset :: BuiltinFunction
reset [MalAtomicCell (MkMalAtom ref), newVal] = liftIO $ atomically (swapTVar ref newVal)
reset xs = liftIO $ throwIO (InvalidArgs "reset!" xs (Just "expected an atom and a new value"))

-- | 'swap' applies a function to the current value inside an atom and sets its new
-- value to the result.
swap :: BuiltinFunction
swap (
    MalAtomicCell (MkMalAtom ref):
    (MalTailRecFunction (MkMalTailRecFunction _ _ _ (MkMalFunction _ func))):
    xs
    ) = do
     oldVal <- liftIO $ readTVarIO ref
     newVal <- func (oldVal:xs)
     liftIO $ atomically (swapTVar ref newVal)
swap xs = liftIO $ throwIO (InvalidArgs "swap!" xs Nothing)
