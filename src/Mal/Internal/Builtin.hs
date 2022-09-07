module Mal.Internal.Builtin where

import           Mal.Error
import           Mal.Internal.Environment (MalFunction)
import           Mal.Internal.Types

-- TODO: handle multiple args
(<+>) :: MalFunction
(<+>) scope (MalAtom (MalNumber x): MalAtom (MalNumber y):_) = pure $ mkMalNumber (x + y)
(<+>) _ xs = Left $ InvalidArgs "+" xs
