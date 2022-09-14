{-# LANGUAGE ScopedTypeVariables #-}
{-|
    The builtin functions of the Mal programming language.
-}
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
    cons,
    concat,
    nth,
    first,
    rest,
    map',

    -- * IO functions
    slurp,

    -- * Atom functions
    atom,
    isAtom,
    deref,
    reset,
    swap,

    -- * Misc
    quasiquote,

    -- * Vector functions
    vec,

    -- * Exception related functions
    throw',

    -- * Predicate functions
    isNil,
    isTrue,
    isFalse,
    isSymbol,
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
import           Control.Lens               hiding (cons)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, asks)
import           Data.List                  (foldl', foldl1')
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           System.IO                  (readFile')

type BuiltinFunction = [MalType] -> ReaderT MalEnv IO MalType

builtins :: MalScope
builtins =
  Env.fromList $
    map
      (\(sym, fn) -> (sym, mkMalFunction sym fn))
      [   ("+", plus)
        , ("-", sub)
        , ("/", quot)
        , ("*", mult)
        , ("list", list)
        , ("list?", isList)
        , ("empty?", isEmpty)
        , ("count", count)
        , ("cons", cons)
        , ("concat", concat')
        , ("nth", nth)
        , ("first", first)
        , ("rest", rest)
        , ("map", map')
        , ("=", eq)
        , ("<", lessThan)
        , ("<=", lessThanEq)
        , (">", greaterThan)
        , (">=", greaterThanEq)
        , ("prn", prn)
        , ("str", str)
        , ("println", println)
        , ("read-string", readString)
        , ("slurp", slurp)
        , ("atom", atom)
        , ("atom?", isAtom)
        , ("deref", deref)
        , ("reset!", reset)
        , ("swap!", swap)
        , ("vec", vec)
        , ("throw", throw')
        , ("nil?", isNil)
        , ("true?", isTrue)
        , ("false?", isFalse)
        , ("symbol?", isSymbol)
      ]

-- Arithmetic functions
reduceMalNumbers :: String -> (Int -> Int -> Int) -> [MalType] -> ReaderT MalEnv IO MalType
reduceMalNumbers funcName f xs = pure $ foldl1' go xs
  where
    go (MalNumber acc) (MalNumber x) = mkMalNumber $ f acc x
    go _ x                           = throw $ InvalidArgs funcName [x] Nothing

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
count xs                       = throwInvalidArgs' "count" xs "expected a list"

-- | Prepend an element to a list.
--
-- >>> (cons 12 (list 1 2 3))
-- (12 1 2 3)
cons :: BuiltinFunction
cons [mt, MalList (MkMalList xs)] = pure $ mkMalList (mt:xs)
cons [mt, MalVec (MkMalVector vs)] = pure $ mkMalList (mt : V.toList vs)
cons xs = throwInvalidArgs' "concat" xs "expected a thing and a list"

-- | 'concat' concatenates the provided lists.
--
-- >>> (concat (list 1 2) (list 3 4))
-- (1 2 3 4)
concat' :: BuiltinFunction
concat' =  pure . mkMalList . flatten []
    where
        flatten :: [MalType] -> [MalType] -> [MalType]
        flatten acc (MalList (MkMalList xs):rest') = flatten (acc ++ xs) rest'
        flatten acc (MalVec (MkMalVector vs):rest') = flatten (acc ++ V.toList vs) rest'
        flatten acc []                            = acc
        flatten _ xs = throw (InvalidArgs "concat" xs $ Just "expected lists")

-- | 'nth' returns the element at the specified position in the provided list or
-- vector. Throws IndexOutOfBounds.
nth :: BuiltinFunction
nth [MalList (MkMalList xs), MalNumber idx] =
    if idx >= length xs then liftIO $ throwIO (IndexOutOfBounds idx $ length xs)
    else pure $ xs !! idx
nth [MalVec (MkMalVector vs), MalNumber idx] =
    if idx >= V.length vs then liftIO $ throwIO (IndexOutOfBounds idx $ V.length vs)
    else pure (vs V.! idx)
nth xs = throwInvalidArgs' "nth" xs "expected a list or a vector"

-- | 'first' returns the first element of the provided list or vector, or nil if they
-- are empty.
first :: BuiltinFunction
first [MalList (MkMalList xs)] = pure $ if null xs then mkMalNil else head xs
first [MalVec (MkMalVector vs)] = pure $ if V.null vs then mkMalNil else V.head vs
first xs = throwInvalidArgs' "first" xs "expected a list or a vector"

-- | 'rest' returns all but the first element of the provided list or vector, or nil if they
-- are empty.
rest :: BuiltinFunction
rest [MalList (MkMalList xs)] = pure $ if null xs then mkMalList [] else mkMalList $ tail xs
rest [MalVec (MkMalVector vs)] = pure $ if V.null vs then mkMalList [] else MalVec $ MkMalVector (V.tail vs)
rest xs = throwInvalidArgs' "rest" xs "expected a list or a vector"

-- | 'map' takes a list and returns a new list that is the result of applying the supplied function
-- to every element of the original list.
map' :: BuiltinFunction
map' [MalTailRecFunction f, MalList (MkMalList xs)] = mkMalList <$> mapM ((f ^. tailRecFunction . fBody) . (:[])) xs
map' [MalFunction f, MalList (MkMalList xs)] = mkMalList <$> mapM ((f ^. fBody) . (:[])) xs
map' xs = throwInvalidArgs' "map" xs "expected a function and a list"

-- Logic functions

compareNumbers :: String -> (Int -> Int -> Bool) -> BuiltinFunction
compareNumbers _ cmp [MalNumber x, MalNumber y] = liftMalType $ cmp x y
compareNumbers funcName _ xs = liftIO $ throwIO (InvalidArgs funcName xs (Just "expected two numbers"))

-- | 'eq' returns true if the provided arguments are equal.
eq :: BuiltinFunction
eq [x, y] = liftMalType $ x == y
eq xs = liftIO $ throwIO (InvalidArgs "eq" xs (Just "expected two arguments"))

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
readString [MalString form] = do
    filename <- asks interpreterFilename
    pure . parse (Just filename) $ T.pack form
readString xs = throw $ InvalidArgs "read-string" xs (Just "expected a string")

-- IO functions

-- | 'slurp' takes a string as its only argument, treats it as a @FilePath@ and
-- returns the contents of the corresponding file.
slurp :: BuiltinFunction
slurp [MalString filename] = do
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
isAtom [MalAtom _] = liftMalType True
isAtom _           = liftMalType False

-- | 'deref' returns the value inside an atom.
deref :: BuiltinFunction
deref [MalAtom ref]= liftIO $ readTVarIO ref
deref xs = liftIO $ throwIO (InvalidArgs "deref" xs (Just "expected a single argument"))

-- | 'reset!' swaps the value inside an atom.
reset :: BuiltinFunction
reset [MalAtom ref, newVal] = liftIO $ atomically (swapTVar ref newVal)
reset xs = liftIO $ throwIO (InvalidArgs "reset!" xs (Just "expected an atom and a new value"))

-- | 'swap' applies a function to the current value inside an atom and sets its new
-- value to the result.
swap :: BuiltinFunction
swap (
    MalAtom ref:
    MalTailRecFunction function:
    xs
    ) = do
     oldVal <- liftIO $ readTVarIO ref
     newVal <- function ^. tailRecFunction . fBody $ oldVal:xs
     liftIO $ atomically (swapTVar ref newVal)
swap xs = liftIO $ throwIO (InvalidArgs "swap!" xs Nothing)

-- Misc

-- | 'quasiquote' quasiquotes an expression.
--
-- ...
--
-- Plz don't ask more, I honestly don't know WTF is going on down there.
-- https://github.com/kanaka/mal/blob/master/process/guide.md#step-7-quoting
--
quasiquote :: BuiltinFunction
quasiquote [MalList (MkMalList ["unquote", ast])] = pure ast
quasiquote [MalList (MkMalList ast)] = go ast
    where
        go :: BuiltinFunction
        go (MalList (MkMalList ["splice-unquote", elt]):rest') = do
            result <- go rest'
            -- This assumes that elt will eventually resolve to a list.
            pure $ mkMalList ["concat", elt, result]
        go (elt:ys) = do
            result <- quasiquote [elt]  -- Quasiquote elt
            rest' <- go ys               -- Process the rest
            pure $ mkMalList ["cons", result, rest']
        -- If the ast is empty just return it as is.
        go [] = pure $ mkMalList []
quasiquote [sym@(MalSymbol _)] = pure $ mkMalList ["quote", sym]
quasiquote [m@(MalMap _)] = pure $ mkMalList ["quote", m]
quasiquote [ast]                                                      = pure ast
quasiquote xs = liftIO $ throwIO (InvalidArgs "quasiquote" xs Nothing)

-- Vector functions
vec :: BuiltinFunction
vec [MalList (MkMalList xs)] = pure $ mkMalVector xs
vec [vs@(MalVec _)] = pure vs
vec xs = liftIO $ throwIO (InvalidArgs "vec" xs (Just "expected a list"))

-- Exception related functions

throw' :: BuiltinFunction
throw' [mt] = liftIO . throwIO . UserGeneratedError $ mt
throw' xs   = liftIO $ throwIO (InvalidArgs "throw" xs Nothing)

 -- Predicate functions

 -- | 'isNil' returns whether its argument is nil.
isNil :: BuiltinFunction
isNil [x] = liftMalType . (== MalNil) $ x
isNil xs  = throwInvalidArgs "nil?" xs

isTrue :: BuiltinFunction
isTrue [MalBool True] = liftMalType True
isTrue [_]            = liftMalType False
isTrue xs             = throwInvalidArgs "true?" xs

isFalse :: BuiltinFunction
isFalse [MalBool False] = liftMalType True
isFalse [_]             = liftMalType False
isFalse xs              = throwInvalidArgs "false?" xs

isSymbol :: BuiltinFunction
isSymbol [MalSymbol _] = liftMalType False
isSymbol [_]           = liftMalType False
isSymbol xs            = throwInvalidArgs "symbol?" xs
