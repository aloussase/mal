{-# LANGUAGE ScopedTypeVariables #-}

module Mal.Internal.Interpreter (eval) where

import           Mal.Error
import qualified Mal.Internal.Builtin       as B
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Types

import           Control.Exception          (catch, evaluate, throw, throwIO)
import           Control.Monad              (liftM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Either.Combinators    (maybeToRight)
import           Data.IORef
import           Data.Map                   (Map, (!?))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V

data MalEnv = MkMalEnv
    { builtins :: MalScope
    , scope    :: IORef MalScope
    }

type Interpreter = ReaderT MalEnv IO MalType

evalAst :: MalType -> Interpreter
evalAst sym@(MalAtom (MalSymbol s)) = do
    env     <- asks scope >>=  liftIO . readIORef
    pure $ Env.find env s

evalAst (MalList (MkMalList xs)) = mkMalList <$> traverse eval' xs
evalAst (MalVec (MkMalVec vs))   = mkMalVector <$> traverse eval' (V.toList vs)
evalAst (MalMap (MkMalMap m))    = do
    let (ks, vs) = unzip . M.toList $ m
    vs' <- traverse eval' vs
    pure $ MalMap (MkMalMap (M.fromList (zip ks vs')))

evalAst ast = pure ast

eval' :: MalType -> Interpreter
eval' xs@(MalList (MkMalList (x:_))) = evalAst xs >>= evalCall
eval' ast                            = evalAst ast

eval :: MalType -> IO MalType
eval ast =  do
    env <- newIORef Env.empty { parent = Just builtins }
    runReaderT (eval' ast) (MkMalEnv builtins env)
    where
        builtins = Env.fromList
            [ ("+", mkMalFunction "+" B.plus)
            , ("-", mkMalFunction "-" B.sub)
            , ("/", mkMalFunction "/" B.quot)
            , ("*", mkMalFunction "*" B.mult)
            ]

evalCall :: MalType -> Interpreter
evalCall (MalList (MkMalList (MalFunction (MkMalFunction _ func):ys))) = do
    scope' <- asks scope >>= liftIO . readIORef
    pure $ func scope' ys
evalCall t = liftIO $ throwIO (NotAFunction t)
