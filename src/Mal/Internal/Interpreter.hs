{-| A tree-walk interpreter for Mal programs. -}
{-# LANGUAGE ScopedTypeVariables #-}

module Mal.Internal.Interpreter (eval) where

import           Mal.Error
import qualified Mal.Internal.Builtin       as B
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Util          (pairs)
import           Mal.Types

import           Control.Exception          (evaluate, throw, throwIO)
import           Control.Monad              (forM_, when)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Data.IORef                 (IORef, modifyIORef', newIORef,
                                             readIORef, writeIORef)
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
    currentScope <- asks scope
    if isTruthy result then liftIO $ eval currentScope trueBranch
    else liftIO $ eval currentScope (fromMaybe mkMalNil falseBranch)

evalAst :: MalType -> Interpreter
evalAst (MalAtom (MalSymbol s)) = do
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
    withScope (Env.empty { scopeParent = Just currentScope }) $ do
        letScope <- asks scope

        forM_ (pairs bindings) $ \(MalAtom (MalSymbol k), v) -> do
            evaledValue <- eval' v
            liftIO $ modifyIORef' letScope (Env.insert k evaledValue)

        -- This let's us do TCO. The alternative would be to do
        -- >>> eval' body
        liftIO $ eval letScope (mkMalList $ mkMalSymbol "do" :body)

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
            currentScope <- asks scope >>= liftIO . readIORef
            let newScope = Env.empty { scopeParent = Just currentScope
                                     , scopeBindings = M.fromList $ mkFnBindings params args}
            -- Eval the function body in the new environment.
            withScope newScope (eval' body)

    currentScope <- asks scope
    let (MalFunction function) = mkMalFunction "lambda" closure
    pure $ mkMalTailRecFunction body params currentScope function

eval' xs@(MalList (MkMalList (_:_))) = evalAst xs >>= evalCall
eval' ast                            = evalAst ast

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
    when (env == Env.empty) $
        modifyIORef' initialScope (\s -> s { scopeParent = Just interpreterBuiltins })
    -- TODO: The MalEnv does not need a reference to the builtins.
    runReaderT (eval' ast) (MkMalEnv interpreterBuiltins initialScope)
    where
        interpreterBuiltins = Env.fromList
            [ ("+", mkMalFunction "+" B.plus)
            , ("-", mkMalFunction "-" B.sub)
            , ("/", mkMalFunction "/" B.quot)
            , ("*", mkMalFunction "*" B.mult)
            , ("list", mkMalFunction "list" B.list)
            , ("list?", mkMalFunction "list?" B.isList)
            , ("empty?", mkMalFunction "empty?" B.isEmpty)
            , ("count", mkMalFunction "count" B.count)
            , ("=", mkMalFunction "=" B.eq)
            , ("<", mkMalFunction "<" B.lessThan)
            , ("<=", mkMalFunction "<=" B.lessThanEq)
            , (">", mkMalFunction ">" B.greaterThan)
            , (">=", mkMalFunction ">=" B.greaterThanEq)
            , ("prn", mkMalFunction "prn" B.prn)
            , ("str", mkMalFunction "str" B.str)
            , ("println", mkMalFunction "println" B.println)
            ]

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
    newEnv <- liftIO $ flip MkMalScope (M.fromList $ mkFnBindings params args) . Just <$> readIORef env
    liftIO $ newIORef newEnv >>= flip eval body

evalCall (MalList (MkMalList (x:_))) = liftIO $ throwIO (NotAFunction x)
evalCall _ = undefined
