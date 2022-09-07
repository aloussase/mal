module Mal.Internal.Types where

data MalAtom =
        MalSymbol String
        | MalNumber Int
        | MalString String
        | MalBool Bool
        | MalNil
    deriving (Eq)

data MalType = MalAtom MalAtom | MalList [MalType]
    deriving (Eq)

instance Show MalAtom where
    show (MalSymbol s)   = s
    show (MalNumber n)   = show n
    show (MalString s)   = s
    show (MalBool True)  = "#t"
    show (MalBool False) = "#f"
    show MalNil          = "nil"

instance Show MalType where
    show (MalAtom a)      = show a
    show (MalList (x:xs)) = mconcat
        ["("
        , show x
        , " "
        , show $ MalList xs
        , ")"]
    show (MalList [])     = "()"
