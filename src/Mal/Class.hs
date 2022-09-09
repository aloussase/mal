{-| Classes useful for using mal as an API. -}
{-# LANGUAGE FlexibleInstances #-}

module Mal.Class where

import           Control.Monad.Trans.Reader (ReaderT)
import           Mal.Types

-- | Convert the provided value to a 'MalType' and lift it into the proper @Monad@.
liftMalType :: (IsMalType a, Monad m) => a -> m MalType
liftMalType = pure . asMalType

-- | Class for types that are convertible to mal types.
class IsMalType a where asMalType :: a -> MalType

-- | Tuples of a function name and body are a valid MalFunction.
instance IsMalType (String, [MalType] -> ReaderT MalEnv IO MalType) where
    asMalType = uncurry mkMalFunction

instance IsMalType Bool where
    asMalType = mkMalBool

instance IsMalType Int where
    asMalType = mkMalNumber

instance IsMalType [MalType] where
    asMalType = mkMalList
