{-|
 Types representing the ast and environment of Mal.
 -}
module Mal.Types (
    -- * Type class for types that can convert to @MalType@
      MalListLike (..)
    -- * Mal data types
    , MalAtom (..)
    , MalFunction (..)
    , MalList (..)
    , MalMap (..)
    , MalType (..)
    , MalVec (..)
    , MalTailRecFunction (..)
    -- * Env things
    , MalEnv (..)
    , MalScope (..)
    , MalFilename (..)
    -- * Smart constructors
    , mkMalBool
    , mkMalFunction
    , mkMalTailRecFunction
    , mkMalList
    , mkMalMap
    , mkMalNil
    , mkMalNumber
    , mkMalString
    , mkMalSymbol
    , mkMalVector
    , mkMalAtom
) where

import           Mal.Internal.Util          (pairs)

import           Control.Concurrent.STM     (TVar, newTVarIO, readTVarIO)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Function              (on)
import           Data.IORef                 (IORef)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.String                (IsString (..))
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           System.IO.Unsafe           (unsafePerformIO)


-- | A class for @MalType@s that can naturally convert to lists.
class MalListLike a where toList :: a -> [MalType]

-- Mal data types

-- | The vector data type in Mal.
newtype MalVec = MkMalVec (Vector MalType) deriving (Show, Eq, Ord)

-- | The workhorse data type in Mal.
newtype MalList = MkMalList [MalType] deriving (Show, Eq, Ord)

-- | The hash-map data type in Mal.
newtype MalMap = MkMalMap (Map MalType MalType) deriving (Show, Eq, Ord)

instance MalListLike MalVec  where toList (MkMalVec vs) = V.toList vs
instance MalListLike MalList where toList (MkMalList xs) = xs
instance MalListLike MalMap where
    toList (MkMalMap m) = M.foldlWithKey' (\xs k v -> k:v:xs) [] m

showListLike :: (MalListLike a) => String -> String -> a -> String
showListLike start end xs = mconcat [start, unwords . map show . toList $ xs, end]

-- | A function in Mal consists of the function's name and a closure.
data MalFunction = MkMalFunction
    { fName :: String
    , fBody :: [MalType] -> ReaderT MalEnv IO MalType
    }

instance Show MalFunction where show f = mconcat ["<fn: ", fName f, ">"]
instance Eq MalFunction   where (==) = (==) `on` fName
instance Ord MalFunction  where compare = compare `on` fName

-- | A tail recursive function.
data MalTailRecFunction = MkMalTailRecFunction
    { tailRecBody     :: MalType        -- ^ The function's body
    , tailRecParams   :: [MalType]      -- ^ The function's parameter list
    , tailRecEnv      :: IORef MalScope -- ^ A reference to the interpreter scope at the time of function creation
    , tailRecFunction :: MalFunction    -- ^ No idea LOL
    }
    deriving (Eq)

instance Ord MalTailRecFunction where compare = compare `on` tailRecFunction

-- | The basic unit of state in Mal, inspired by Clojure atoms.
newtype MalAtom = MkMalAtom (TVar MalType) deriving (Eq)

instance Show MalAtom where show (MkMalAtom ref) = mconcat ["<atom: ", show $ unsafePerformIO (readTVarIO ref), ">"]
instance Ord MalAtom  where compare = error "atoms are not comparable"

-- | A data type in the Mal language.
data MalType =
          MalSymbol String
        | MalNumber Int
        | MalString String
        | MalBool Bool
        | MalNil
        | MalList MalList
        | MalVec MalVec
        | MalMap MalMap
        | MalFunction MalFunction
        | MalTailRecFunction MalTailRecFunction
        | MalAtomicCell MalAtom
    deriving (Eq, Ord)

instance IsString MalType where fromString = MalSymbol

-- Env things

newtype MalFilename = MkMalFilename FilePath

-- | The environment for the interpreter.
data MalEnv = MkMalEnv
    { interpreterScope    :: IORef MalScope
    , interpreterFilename :: MalFilename
    }

-- | A scope where bindings run happily in the meadows.
data MalScope = MkMalScope
    { scopeParent   :: Maybe (IORef MalScope)
    , scopeBindings :: Map String MalType
    }
    deriving (Eq)

-- Smart constructors

-- | Make a 'MalFunction' from the provided function name and closure.
mkMalFunction :: String -> ([MalType] -> ReaderT MalEnv IO MalType) -> MalType
mkMalFunction name body = MalFunction $ MkMalFunction { fName = name, fBody = body }

mkMalTailRecFunction:: MalType -> [MalType] -> IORef MalScope -> MalFunction -> MalType
mkMalTailRecFunction body params env function =
    MalTailRecFunction $ MkMalTailRecFunction body params env function

-- | Make a Mal string from the provided @String@.
mkMalSymbol :: String -> MalType
mkMalSymbol =  MalSymbol

-- | Make a Mal symbol from the provided @String@.
mkMalString :: String -> MalType
mkMalString = MalString

-- | Make a Mal number from the provided @Int@.
mkMalNumber :: Int -> MalType
mkMalNumber = MalNumber

-- | Make a Mal bool from the provided @Bool@.
mkMalBool :: Bool -> MalType
mkMalBool = MalBool

-- | Make Mal nil.
mkMalNil ::MalType
mkMalNil = MalNil

-- | Make a Mal list from the provided list of 'MalType'.
mkMalList :: [MalType] -> MalType
mkMalList = MalList . MkMalList

-- | Make a Mal list from the provided list of 'MalType'.
mkMalVector :: [MalType] -> MalType
mkMalVector = MalVec . MkMalVec . V.fromList

-- | Make a Mal list from the provided list of 'MalType'.
-- Each pair of successive elements is a key-value pair in the resulting map.
mkMalMap :: [MalType] -> MalType
mkMalMap = MalMap . MkMalMap . M.fromList . pairs

mkMalAtom :: (MonadIO m) => MalType -> m MalType
mkMalAtom t = MalAtomicCell . MkMalAtom <$> liftIO (newTVarIO t)

instance Show MalType where
    show (MalSymbol s)       = s
    show (MalNumber n)       = show n
    show (MalString s)       = mconcat [ "\"", s, "\""]
    show (MalBool True)      = "#t"
    show (MalBool False)     = "#f"
    show MalNil              = "nil"
    show (MalList xs)        = showListLike "(" ")" xs
    show (MalVec vs)         = showListLike "[" "]" vs
    show (MalMap m)          = showListLike "{" "}" m
    show (MalFunction f)     = show f
    show (MalAtomicCell ref) = show ref
    show (MalTailRecFunction (MkMalTailRecFunction body params _ _)) =
        mconcat [ "<fn: "
                , "args: ", show params, "\n"
                , "body: ", show body,   "\n>"
                ]
