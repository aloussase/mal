{-# LANGUAGE TemplateHaskell #-}
{-|
 Types representing the ast and environment of Mal.
 -}
module Mal.Types (
    Interpreter
    -- * Type class for types that can convert to @MalType@
    -- * Mal data types
    , MalFunction (..)
    , MalType (..)
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
    , malVectorToList
    , malMapToList
    -- * Type predicates
    , isKeyword
    -- * Lenses
    , fName
    , fBody
    , fIsMacro
    , tailRecBody
    , tailRecFunction
    , tailRecParams
    , tailRecEnv
) where

import           Mal.Internal.Util          (pairs)

import           Control.Concurrent.STM     (TVar, newTVarIO, readTVarIO)
import           Control.Lens.TH            (makeLenses)
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

type Interpreter = ReaderT MalEnv IO MalType

-- Mal data types

malVectorToList :: Vector MalType -> [MalType]
malVectorToList = V.toList

malMapToList :: Map MalType MalType -> [MalType]
malMapToList = M.foldlWithKey' (\xs k v -> k:v:xs) []

showListLike :: (Show a) => String -> String -> [a] -> String
showListLike start end xs = mconcat [start, unwords $ map show xs, end]

-- | A data type in the Mal language.
data MalType =
          MalSymbol String
        | MalNumber Int
        | MalString String
        | MalKeyword String
        | MalBool Bool
        | MalNil
        | MalList [MalType]
        | MalVector (Vector MalType)
        | MalMap (Map MalType MalType)
        | MalFunction MalFunction
        | MalTailRecFunction MalTailRecFunction
        | MalAtom (TVar MalType)
    deriving (Eq)


instance IsString MalType where fromString = MalSymbol

instance Show MalType where
    show (MalSymbol s)   = s
    show (MalNumber n)   = show n
    show (MalString s)   = mconcat [ "\"", s, "\""]
    show (MalKeyword s)  = ":" <>  s
    show (MalBool True)  = "#t"
    show (MalBool False) = "#f"
    show MalNil          = "nil"
    show (MalList xs)    = showListLike "(" ")" xs
    show (MalVector vs)     = showListLike "[" "]" $ malVectorToList vs
    show (MalMap m)      = showListLike "{" "}" $ malMapToList m
    show (MalFunction f) = show f
    show (MalAtom ref)   = mconcat ["<atom: ", show $ unsafePerformIO (readTVarIO ref), ">"]
    show (MalTailRecFunction (MkMalTailRecFunction body params _ _)) =
        mconcat [ "<fn: "
                , "args: ", show params, "\n"
                , "body: ", show body,   "\n>"
                ]

instance Ord MalType where
    compare (MalSymbol x)          (MalSymbol y)          = compare x y
    compare (MalNumber x)          (MalNumber y)          = compare x y
    compare (MalString x)          (MalString y)          = compare x y
    compare (MalBool x)            (MalBool y)            = compare x y
    compare (MalList x)            (MalList y)            = compare x y
    compare (MalVector x)          (MalVector y)             = compare x y
    compare (MalMap x)             (MalMap y)             = compare x y
    compare (MalFunction x)        (MalFunction y)        = compare x y
    compare (MalTailRecFunction x) (MalTailRecFunction y) = compare x y
    compare (MalKeyword x)         (MalKeyword y)         = compare x y
    compare (MalString x)          (MalKeyword y)         = compare x y
    compare (MalSymbol x)          (MalKeyword y)         = compare x y
    compare (MalKeyword x)         (MalString y)          = compare x y
    compare (MalKeyword x)         (MalSymbol y)          = compare x y
    compare _ _ = error "cant compare this lemons and apples"

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
mkMalFunction name body = MalFunction $ MkMalFunction { _fName = name, _fBody = body, _fIsMacro = False }

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
mkMalList = MalList

-- | Make a Mal list from the provided list of 'MalType'.
mkMalVector :: [MalType] -> MalType
mkMalVector = MalVector . V.fromList

-- | Make a Mal list from the provided list of 'MalType'.
-- Each pair of successive elements is a key-value pair in the resulting map.
mkMalMap :: [MalType] -> MalType
mkMalMap = MalMap . M.fromList . pairs

mkMalAtom :: (MonadIO m) => MalType -> m MalType
mkMalAtom t = MalAtom <$> liftIO (newTVarIO t)

-- Type predicates

isKeyword :: MalType -> Bool
isKeyword (MalKeyword _ ) = True
isKeyword _               = False

-- Function types

-- | A tail recursive function.
data MalTailRecFunction = MkMalTailRecFunction
    { _tailRecBody     :: MalType        -- ^ The function's body
    , _tailRecParams   :: [MalType]      -- ^ The function's parameter list
    , _tailRecEnv      :: IORef MalScope -- ^ A reference to the interpreter scope at the time of function creation
    , _tailRecFunction :: MalFunction    -- ^ No idea LOL
    }
    deriving (Eq)

instance Ord MalTailRecFunction where compare = compare `on` _tailRecFunction

-- | A function in Mal consists of the function's name and a closure.
data MalFunction = MkMalFunction
    { _fName    :: String
    , _fBody    :: [MalType] -> ReaderT MalEnv IO MalType
    , _fIsMacro :: Bool
    }

instance Show MalFunction where show f = mconcat ["<fn: ", _fName f, ">"]
instance Eq MalFunction   where (==) = (==) `on` _fName
instance Ord MalFunction  where compare = compare `on` _fName

makeLenses ''MalFunction
makeLenses ''MalTailRecFunction


