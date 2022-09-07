module Mal.Internal.Interpreter (eval) where

import           Mal.Error
import           Mal.Internal.Builtin       ((<+>))
import           Mal.Internal.Environment   (MalFunction, MalScope)
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Types

import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Either.Combinators    (maybeToRight)
import           Data.IORef
import           Data.Map                   (Map)
import qualified Data.Map                   as M

data MalEnv = MkMalEnv
    { builtins :: Map String MalFunction
    , scope    :: IORef MalScope
    }

type Interpreter = ReaderT MalEnv IO (Either MalError MalType)

evalAst :: MalType -> Interpreter
evalAst (MalAtom (MalSymbol s)) = do
    env <- asks scope >>=  liftIO . readIORef
    pure $ Env.find env s

evalAst (MalList (MkMalList (x:xs))) = do
    fst  <- evalAst x
    rest <- sequence <$> traverse evalAst xs
    pure $ mkMalList <$> ((:) <$> fst <*> rest)

evalAst ast = pure (pure ast)

eval :: MalType -> IO (Either MalError MalType)
eval ast =  do
    env <- newIORef Env.empty
    runReaderT (go ast) (MkMalEnv globals env)

    where
        go :: MalType -> Interpreter
        go xs@(MalList _) = do
            evaledAst <- evalAst xs
            case evaledAst of
                Right ast' -> evalCall ast' >>= pure
                err        -> pure err

        go ast            = evalAst ast

        globals = M.singleton "+" (<+>)

evalCall :: MalType -> Interpreter
evalCall (MalList (MkMalList (y:ys))) = undefined
evalCall t                            = pure $ Left (NotAFunction t)
