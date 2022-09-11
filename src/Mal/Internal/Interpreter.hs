{-| A tree-walk interpreter for Mal programs. -}
{-# LANGUAGE ScopedTypeVariables #-}

module Mal.Internal.Interpreter (eval) where

import           Mal.Error
import qualified Mal.Internal.Builtin       as B
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Util          (pairs)
import           Mal.Types

import           Control.Exception          (throw, throwIO)
import           Control.Monad              (when)
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
    currentScope <- asks scope
    if isTruthy result then liftIO $ eval currentScope trueBranch
    else liftIO $ eval currentScope (fromMaybe mkMalNil falseBranch)

evalAst :: MalType -> Interpreter
evalAst (MalAtom (MalSymbol s)) = do
    env <- asks scope
    liftIO $ Env.find env s
evalAst (MalList (MkMalList xs)) = mkMalList <$> traverse eval' xs
evalAst (MalVec (MkMalVec vs))   = mkMalVector <$> traverse eval' (V.toList vs)
evalAst (MalMap (MkMalMap m))    = do
    let (ks, vs) = unzip . M.toList $ m
    vs' <- traverse eval' vs
    pure $ MalMap (MkMalMap (M.fromList (zip ks vs')))

evalAst ast = pure ast

eval' :: MalType -> Interpreter
eval' (MalList (MkMalList [MalAtom (MalSymbol "def!"), MalAtom (MalSymbol name), val])) = do
    evaledVal   <- eval' val
    globalScope <- asks scope >>= liftIO . Env.getRoot
    liftIO $ modifyIORef' globalScope (Env.insert name evaledVal)
    pure mkMalNil

-- let special form
--
-- Here we have to make a choice: are let bindings evaluated in the current environment or in the
-- new let environment?
--
-- If we go for the former, then the bodies of functions defined in a let binding
-- cannot refer to other variables defined in the same let, as they won't exist in that moment.
--
eval' (MalList (MkMalList (MalAtom (MalSymbol "let*"):MalList (MkMalList bindings):body))) = do
    currentScope <- asks scope
    letBindings  <- mapM (\(MalAtom (MalSymbol k), v) -> (,) k <$> eval' v) (pairs bindings)
    letScope <- liftIO $ newIORef Env.empty { scopeParent = Just currentScope, scopeBindings = M.fromList letBindings }

    -- This let's us do TCO. The alternative would be to do
    -- >>> eval' body
    liftIO $ eval letScope (mkMalList $ mkMalSymbol "do" : body)

-- Do special form
eval' (MalList (MkMalList (MalAtom (MalSymbol "do"):body))) =
    if null body then pure mkMalNil
    else do
        -- I have to do @eval'@ here because evalAst does not recognize special forms.
        mapM_ eval' (init body)
        currentScope <- asks scope
        liftIO $ eval currentScope (last body)

-- If expression
eval' (MalList (MkMalList [MalAtom (MalSymbol "if"), condition, trueBranch])) =
    evalIfStmt condition trueBranch Nothing
eval' (MalList (MkMalList [MalAtom (MalSymbol "if"), condition, trueBranch, falseBranch])) =
    evalIfStmt condition trueBranch (Just falseBranch)

-- fn special form (lambdas)
eval' (MalList (MkMalList [MalAtom (MalSymbol "fn*"), MalList (MkMalList params), body])) = do
    let closure :: [MalType] -> Interpreter
        closure args = do
            -- Create a new environment from the outer scope and bind the function arguments in it.
            currentScope <- asks scope
            closureScope <- liftIO $ newIORef Env.empty { scopeParent = Just currentScope
                                                        , scopeBindings = M.fromList $ mkFnBindings params args
                                                        }

            -- Eval the function body in the new environment.
            liftIO $ eval closureScope body

    currentScope <- asks scope
    let (MalFunction function) = mkMalFunction "lambda" closure
    pure $ mkMalTailRecFunction body params currentScope function

eval' xs@(MalList _) = evalAst xs >>= evalCall
eval' ast = evalAst ast

-- | Bind a list of function names to the corresponding arguments.
-- This handles clojure-style rest params as well.
mkFnBindings :: [MalType] -> [MalType] -> [(String, MalType)]
mkFnBindings =  go []
    where
        go :: [(String, MalType)] -> [MalType] -> [MalType] -> [(String, MalType)]
        go bindings [MalAtom (MalSymbol "&"), MalAtom (MalSymbol rest)] args = (rest, mkMalList args):bindings
        go _ (MalAtom (MalSymbol "&"):_:_) _ = throw $ InvalidSignature "expected only 1 argument after '&'"
        go bindings ((MalAtom (MalSymbol name)):names) (arg:args) = go ((name, arg):bindings) names args
        go bindings _ _ = bindings

-- | 'eval' evaluates the provided 'MalType', using @scope@ as the initial
-- environment.
eval :: IORef MalScope -> MalType -> IO MalType
eval initialScope ast =  do
    env <- readIORef initialScope

    when (env == Env.empty) $ do
        topLevelScope <- newIORef builtins
        modifyIORef' initialScope (\s -> s { scopeParent = Just topLevelScope })

    runReaderT (eval' ast) (MkMalEnv initialScope)

    where
        builtins = Env.insert "eval" (mkMalFunction "eval" builtinEval) B.builtins

        builtinEval :: [MalType] -> Interpreter
        builtinEval [ast'] = do
            globalScope <- asks scope >>= liftIO . Env.getRoot
            liftIO $ eval globalScope ast'
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
--
evalCall :: MalType -> Interpreter
evalCall (MalList (MkMalList (MalFunction (MkMalFunction _ func):args))) = func args
evalCall (MalList (MkMalList (MalTailRecFunction (MkMalTailRecFunction body params env _func):args))) = do
    let argBindings = M.fromList $ mkFnBindings params args
        functionScope = Env.empty { scopeParent = Just env, scopeBindings = argBindings }
    liftIO $ newIORef functionScope >>= flip eval body
evalCall (MalList (MkMalList (x:_))) = liftIO $ throwIO (NotAFunction x)
evalCall _ = undefined
