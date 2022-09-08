{-# LANGUAGE ScopedTypeVariables #-}

module Mal.Internal.Interpreter (eval) where

import           Mal.Class
import           Mal.Error
import qualified Mal.Internal.Builtin       as B
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Util          (pairs)
import           Mal.Types

import           Control.Exception          (catch, evaluate, throw, throwIO)
import           Control.Monad              (forM, forM_, liftM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import           Data.Either.Combinators    (maybeToRight)
import           Data.IORef                 (IORef, modifyIORef', newIORef,
                                             readIORef, writeIORef)
import           Data.Map                   (Map, (!?))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V

type Interpreter = ReaderT MalEnv IO MalType

isFalsey :: MalType -> Bool
isFalsey (MalAtom (MalBool False)) = True
isFalsey (MalAtom MalNil)          = True
isFalsey _                         = False

isTruthy :: MalType -> Bool
isTruthy = not . isFalsey

withScope :: MalScope -> Interpreter -> Interpreter
withScope newScope action = do
    !scopeRef <- asks scope
    !oldScope <- liftIO $ readIORef scopeRef
    liftIO (writeIORef scopeRef newScope) *> action <* liftIO (writeIORef scopeRef oldScope)

evalIfStmt :: MalType -> MalType -> Maybe MalType -> Interpreter
evalIfStmt condition trueBranch falseBranch = do
    result <- eval' condition
    if isTruthy result then eval' trueBranch
    else eval' (fromMaybe mkMalNil falseBranch)

evalAst :: MalType -> Interpreter
evalAst sym@(MalAtom (MalSymbol s)) = do
    env <- asks scope >>= liftIO . readIORef
    liftIO $ evaluate (Env.find env s)

evalAst (MalList (MkMalList xs)) = mkMalList <$> traverse eval' xs
evalAst (MalVec (MkMalVec vs))   = mkMalVector <$> traverse eval' (V.toList vs)
evalAst (MalMap (MkMalMap m))    = do
    let (ks, vs) = unzip . M.toList $ m
    vs' <- traverse eval' vs
    pure $ MalMap (MkMalMap (M.fromList (zip ks vs')))

evalAst ast = pure ast

eval' :: MalType -> Interpreter
eval' (MalList (MkMalList [MalAtom (MalSymbol "def!"), MalAtom (MalSymbol name), val])) = do
    evaledVal <- eval' val
    currentScope <- asks scope
    liftIO $ modifyIORef' currentScope (Env.insert name evaledVal)
    pure mkMalNil

-- let special form
eval' (MalList (MkMalList (MalAtom (MalSymbol "let*"):MalList (MkMalList bindings):body))) = do
    currentScope <- asks scope >>= liftIO . readIORef
    withScope (Env.empty { parent = Just currentScope }) $ do
        forM_ (pairs bindings) $ \(MalAtom (MalSymbol k), v) -> do
            evaledValue <- eval' v
            currentScope <- asks scope
            liftIO $ modifyIORef' currentScope (Env.insert k evaledValue)
        foldr1 (>>) $ map eval' body

-- Do special form
eval' (MalList (MkMalList (MalAtom (MalSymbol "do"):body))) = foldr1 (>>) $ map eval' body

-- If expression
eval' (MalList (MkMalList [MalAtom (MalSymbol "if"), condition, trueBranch])) =
    evalIfStmt condition trueBranch Nothing
eval' (MalList (MkMalList [MalAtom (MalSymbol "if"), condition, trueBranch, falseBranch])) =
    evalIfStmt condition trueBranch (Just falseBranch)

-- fn special form (lambdas)
eval' (MalList (MkMalList [MalAtom (MalSymbol "fn*"), MalList (MkMalList params), body@(MalList _)])) = do
    let closure :: [MalType] -> Interpreter
        closure args = do
            -- Create a new environment from the outer scope and bind the function arguments in it.
            currentScope <- asks scope >>= liftIO . readIORef

            let paramNames  = map (\(MalAtom (MalSymbol param)) -> param) params
                newScope = currentScope { parent = Just currentScope
                                        , bindings = M.fromList (zip paramNames args)
                                        }

            -- Eval the function body in the new environment.
            withScope newScope (eval' body)

    pure $ mkMalFunction "lambda" closure

eval' xs@(MalList (MkMalList (x:_))) = evalAst xs >>= evalCall
eval' ast                            = evalAst ast

eval :: IORef MalScope -> MalType -> IO MalType
eval scope ast =  do
    modifyIORef' scope (\s -> s { parent = Just builtins })
    runReaderT (eval' ast) (MkMalEnv builtins scope)
    where
        builtins = Env.fromList
            [ ("+", mkMalFunction "+" B.plus)
            , ("-", mkMalFunction "-" B.sub)
            , ("/", mkMalFunction "/" B.quot)
            , ("*", mkMalFunction "*" B.mult)
            , ("list", mkMalFunction "list" B.list)
            , ("list?", mkMalFunction "list?" B.isList)
            , ("empty?", mkMalFunction "empty?" B.isEmpty)
            , ("count", mkMalFunction "count" B.count)
            ]

evalCall :: MalType -> Interpreter
evalCall (MalList (MkMalList (MalFunction (MkMalFunction _ func):ys))) = func ys
evalCall t = liftIO $ throwIO (NotAFunction t)
