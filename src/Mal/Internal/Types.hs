module Mal.Internal.Types where

import           Mal.Internal.Util (pairs)

import           Data.Map          (Map)
import qualified Data.Map          as M
import           Data.Vector       (Vector)
import qualified Data.Vector       as V

newtype MalVec = MkMalVec (Vector MalType) deriving (Show, Eq, Ord)
newtype MalList = MkMalList [MalType] deriving (Show, Eq, Ord)
newtype MalMap = MkMalMap (Map MalType MalType) deriving (Show, Eq, Ord)

-- | A class for @MalType@s that can naturally convert to lists.
class MalListLike a where
    toList :: a -> [MalType]

instance MalListLike MalVec where toList (MkMalVec vs) = V.toList vs
instance MalListLike MalList where toList (MkMalList xs) = xs
instance MalListLike MalMap where
    toList (MkMalMap m) = M.foldlWithKey' (\xs k v -> k:v:xs) [] m

showListLike :: (MalListLike a) => String -> String -> a -> String
showListLike start end xs = mconcat [start, unwords . map show . toList $ xs, end]

data MalAtom =
        MalSymbol String
        | MalNumber Int
        | MalString String
        | MalBool Bool
        | MalNil
    deriving (Eq, Ord)

data MalType =
        MalAtom MalAtom
        | MalList MalList
        | MalVec MalVec
        | MalMap MalMap
    deriving (Eq, Ord)

-- | Smart constructor for making Mal Symbol.
mkMalSymbol :: String -> MalType
mkMalSymbol = MalAtom . MalSymbol

-- | Smart constructor for making Mal String.
mkMalString :: String -> MalType
mkMalString = MalAtom . MalString

-- | Smart constructor for making Mal Number.
mkMalNumber :: Int -> MalType
mkMalNumber = MalAtom . MalNumber

-- | Smart constructor for making Mal Bool.
mkMalBool :: Bool -> MalType
mkMalBool = MalAtom . MalBool

-- | Smart constructor for making Mal Nil.
mkMalNil ::MalType
mkMalNil = MalAtom MalNil

-- | Smart constructor for making @MalList@s.
mkMalList :: [MalType] -> MalType
mkMalList = MalList . MkMalList

mkMalVector :: [MalType] -> MalType
mkMalVector = MalVec . MkMalVec . V.fromList

mkMalMap :: [MalType] -> MalType
mkMalMap = MalMap . MkMalMap . M.fromList . pairs

instance Show MalAtom where
    show (MalSymbol s)   = s
    show (MalNumber n)   = show n
    show (MalString s)   = mconcat [ "\"", s, "\""]
    show (MalBool True)  = "#t"
    show (MalBool False) = "#f"
    show MalNil          = "nil"

instance Show MalType where
    show (MalAtom a)  = show a
    show (MalList xs) = showListLike "(" ")" xs
    show (MalVec vs)  = showListLike "[" "]" vs
    show (MalMap m)   = showListLike "{" "}" m
