{-# LANGUAGE ScopedTypeVariables #-}

-- | A tree-walk interpreter for Mal programs.
module Mal.Internal.Interpreter (eval) where

import           Mal.Error
import qualified Mal.Internal.Builtin       as B
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Util          (pairs)
import           Mal.Types

import           Control.Exception          (throw, throwIO)
import           Control.Monad              (forM, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Data.IORef                 (IORef, modifyIORef', newIORef,
                                             readIORef)
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

evalIfStmt :: MalType -> MalType -> Maybe MalType -> Interpreter
evalIfStmt condition trueBranch falseBranch = do
  result <- eval' condition
  currentScope <- asks interpreterScope
  if isTruthy result
    then liftIO $ eval Nothing currentScope trueBranch
    else liftIO $ eval Nothing currentScope (fromMaybe mkMalNil falseBranch)

evalAst :: MalType -> Interpreter
evalAst (MalAtom (MalSymbol s)) = asks interpreterScope >>= liftIO . flip Env.find s
evalAst (MalList (MkMalList xs)) = mkMalList <$> traverse eval' xs
evalAst (MalVec (MkMalVec vs)) = mkMalVector <$> traverse eval' (V.toList vs)
evalAst (MalMap (MkMalMap m)) = do
  let (ks, vs) = unzip . M.toList $ m
  vs' <- traverse eval' vs
  pure $ MalMap (MkMalMap (M.fromList (zip ks vs')))
evalAst ast = pure ast

eval' :: MalType -> Interpreter
eval' (MalList (MkMalList ["def!", MalAtom (MalSymbol name), val])) = do
  evaledVal <- eval' val
  globalScope <- asks interpreterScope >>= liftIO . Env.getRoot
  liftIO $ modifyIORef' globalScope (Env.insert name evaledVal)
  pure mkMalNil

-- let special form
--
-- Here we have to make a choice: are let bindings evaluated in the current environment or in the
-- new let environment?
--
-- If we go for the former, then the bodies of functions defined in a let binding
-- cannot refer to other variables defined in the same let, as they won't exist in that moment.
-- This is how Clojure's let works.
--
-- Clojure has another macro called letfn that behaves like the latter and like the let we will
-- be implementing.
--
eval' (MalList (MkMalList ("let*": MalList (MkMalList bindings) : body))) = do
  currentScope <- asks interpreterScope
  letScope <- liftIO $ newIORef Env.empty {scopeParent = Just currentScope}
  letBindings <- liftIO $ forM (pairs bindings) (\(MalAtom (MalSymbol k), v) -> (,) k <$> eval Nothing letScope v)

  liftIO $ modifyIORef' letScope (\s -> s {scopeBindings = M.fromList letBindings})

  -- This let's us do TCO. The alternative would be to do
  -- >>> eval' body
  liftIO $ eval Nothing letScope (mkMalList $ mkMalSymbol "do" : body)

-- Do special form
eval' (MalList (MkMalList ("do" : body))) =
  if null body
    then pure mkMalNil
    else do
      -- I have to do @eval'@ here because evalAst does not recognize special forms.
      mapM_ eval' (init body)
      currentScope <- asks interpreterScope
      liftIO $ eval Nothing currentScope (last body)

-- If expression
eval' (MalList (MkMalList ["if", condition, trueBranch])) =
  evalIfStmt condition trueBranch Nothing
eval' (MalList (MkMalList ["if", condition, trueBranch, falseBranch])) =
  evalIfStmt condition trueBranch (Just falseBranch)

-- fn* special form (lambdas)
eval' (MalList (MkMalList ["fn*", MalList (MkMalList params), body])) = do
  let closure :: [MalType] -> Interpreter
      closure args = do
        -- Create a new environment from the outer scope and bind the function arguments in it.
        currentScope <- asks interpreterScope
        closureScope <-
          liftIO $
            newIORef
              Env.empty
                { scopeParent = Just currentScope,
                  scopeBindings = M.fromList $ mkFnBindings "lambda" params args
                }

        -- Eval the function body in the new environment.
        liftIO $ eval Nothing closureScope body

  currentScope <- asks interpreterScope
  let (MalFunction function) = mkMalFunction "lambda" closure
  pure $ mkMalTailRecFunction body params currentScope function

-- quote special form
eval' (MalList (MkMalList ["quote", mt])) = pure mt

-- quasiquote special form
eval' (MalList (MkMalList ("quasiquote":ast))) = B.quasiquote ast >>= eval'

-- Here we probably have a function call.
eval' xs@(MalList _) = evalAst xs >>= evalCall

-- Otherwise just return the evaluated ast.
eval' ast = evalAst ast

-- | 'eval' evaluates the provided 'MalType', using @scope@ as the initial
-- environment.
eval :: Maybe MalFilename -> IORef MalScope -> MalType -> IO MalType
eval filename initialScope ast = do
  env <- readIORef initialScope

  when (env == Env.empty) $ do
    topLevelScope <- newIORef builtins
    modifyIORef' initialScope (\s -> s {scopeParent = Just topLevelScope})

  runReaderT (eval' ast) (MkMalEnv initialScope $ fromMaybe (MkMalFilename "<repl>") filename)
  where
    builtins = Env.insert "eval" (mkMalFunction "eval" builtinEval) B.builtins

    builtinEval :: [MalType] -> Interpreter
    builtinEval [ast'] = do
      globalScope <- asks interpreterScope >>= liftIO . Env.getRoot
      liftIO $ eval Nothing globalScope ast'
    builtinEval xs = liftIO $ throwIO (InvalidArgs "eval" xs $ Just "expected a single argument")

-- | 'evalCall' evaluates a function call.
--
-- If it is a normal function, we just call it with the provided arguments.
--
-- Otherwise, if it is a tail recursive function we bind the function arguments in the
-- stored environment at the time of closure creation and evaluate its body using that
-- environment.
--
-- For anything else, throw an error.
evalCall :: MalType -> Interpreter
evalCall (MalList (MkMalList (MalFunction (MkMalFunction _ func) : args))) = func args
evalCall (MalList (MkMalList (MalTailRecFunction (MkMalTailRecFunction body params env func) : args))) = do
  let argBindings = M.fromList $ mkFnBindings (fName func) params args
      functionScope = Env.empty {scopeParent = Just env, scopeBindings = argBindings}
  liftIO $ newIORef functionScope >>= flip (eval Nothing) body
evalCall (MalList (MkMalList (x : _))) = liftIO $ throwIO (NotAFunction x)
evalCall ast = pure ast

-- | Bind a list of function names to the corresponding arguments.
-- This handles clojure-style rest params as well.
mkFnBindings :: String -> [MalType] -> [MalType] -> [(String, MalType)]
mkFnBindings funcName xs ys =
    if length xs == length ys || any (isSymbol "&") xs then
        go [] xs ys
    else
        throw $ InvalidArgs funcName xs (Just $ mconcat [
                "expected "
              , show (length xs)
              , " argument(s), but got "
              , show (length ys)
              ])
  where
    go :: [(String, MalType)] -> [MalType] -> [MalType] -> [(String, MalType)]
    go bindings ["&", MalAtom (MalSymbol rest)] args = (rest, mkMalList args) : bindings
    go _ ("&": _ : _) _ = throw $ InvalidSignature "expected only 1 argument after '&'"
    go bindings ((MalAtom (MalSymbol name)) : names) (arg : args) = go ((name, arg) : bindings) names args
    go bindings _ _ = bindings

    isSymbol :: String -> MalType -> Bool
    isSymbol sym (MalAtom (MalSymbol s)) = sym == s
    isSymbol _ _                         =  False

