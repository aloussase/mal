{-# LANGUAGE ScopedTypeVariables #-}

-- | A tree-walk interpreter for Mal programs.
module Mal.Internal.Interpreter (eval) where

import           Mal.Error
import           Mal.Internal.Environment   (withScope)
import qualified Mal.Internal.Environment   as Env
import           Mal.Internal.Util          (pairs)
import           Mal.Types

import           Control.Exception          (Handler (Handler), SomeException,
                                             catch, catches, throw, throwIO)
import           Control.Lens
import           Control.Monad              (forM)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (asks, runReaderT)
import           Data.IORef                 (IORef, modifyIORef', newIORef)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V


-- | 'eval' evaluates the provided 'MalType', using @scope@ as the initial
-- environment.
eval :: Maybe MalFilename -> IORef MalScope -> MalType -> IO MalType
eval filename initialScope ast = do
  runReaderT
    (macroexpand initialScope ast >>= eval')
    (MkMalEnv initialScope $ fromMaybe (MkMalFilename "<repl>") filename)

evalAst :: MalType -> Interpreter
evalAst (MalSymbol s) = asks interpreterScope >>= liftIO . flip Env.find s
evalAst (MalList xs) = mkMalList <$> traverse eval' xs
evalAst (MalVector vs) = mkMalVector <$> traverse eval' (V.toList vs)
evalAst (MalMap m) = do
  let (ks, vs) = unzip . M.toList $ m
  vs' <- traverse eval' vs
  pure $ MalMap (M.fromList (zip ks vs'))
evalAst ast = pure ast

eval' :: MalType -> Interpreter

-- def!
--
-- The def! special form binds symbols at the top level scope, meaning it always creates
-- global variables.
--
-- TODO: Consider implementing a `local` special form to define local variables like in Fennel.
--
eval' (MalList ["def!", MalSymbol name, val]) = do
  evaledVal <- eval' val
  globalScope <- asks interpreterScope >>= liftIO . Env.getRoot
  let value = case evaledVal of
        MalTailRecFunction f -> MalTailRecFunction $ f & tailRecFunction . fName .~ name
        _ -> evaledVal
  liftIO $ modifyIORef' globalScope (Env.insert name value)
  pure mkMalNil
eval' (MalList ("def!":_)) = throwSpecialForm "def!"

-- defmacro!
--
-- A macro is just a function marked as a macro. This will allow us to treat it diffrently
-- when the time to evaluate a macro call comes.
--
eval' (MalList ["defmacro!", MalSymbol name, val]) = do
  evaledVal <- eval' val
  case evaledVal of
    (MalTailRecFunction f) -> do
      globalScope <- asks interpreterScope >>= liftIO . Env.getRoot
      liftIO $
        modifyIORef'
          globalScope
          ( Env.insert
              name
              (MalTailRecFunction (f & tailRecFunction . fName .~ name & tailRecFunction . fIsMacro .~ True ))
          )
      pure MalNil
    other -> liftIO $ throwIO (InvalidArgs "defmacro!" [other] Nothing)
eval' (MalList ("defmacro!":_)) = throwSpecialForm "defmacro!"

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
eval' (MalList ("let*" : MalList bindings : body)) = do
  -- Create a new scope for let*
  currentScope <- asks interpreterScope
  letScope <- liftIO $ newIORef Env.empty {scopeParent = Just currentScope}

  -- Create the let* bindings using the let scope.
  --
  -- Here we need scopes to be mutable is functions defined in a let* are to have access to bindings
  -- created later.
  letBindings <- liftIO $ forM (pairs bindings) (\(MalSymbol k, v) -> (,) k <$> eval Nothing letScope v)
  liftIO $ modifyIORef' letScope (\s -> s {scopeBindings = M.fromList letBindings})

  -- This let's us do TCO. The alternative would be to do
  -- >>> eval' body
  liftIO $ eval Nothing letScope (mkMalList $ "do" : body)
eval' (MalList ("let*":_)) = throwSpecialForm "let*"

-- do special form
--
-- Evaluates the forms in its body one at a time and return the result of the last form.
--
eval' (MalList ("do" : body)) =
  if null body
    then pure mkMalNil
    else do
      currentScope <- asks interpreterScope
      liftIO $ last <$> mapM (eval Nothing currentScope) body

-- if special form
eval' (MalList ["if", condition, trueBranch]) = evalIfStmt condition trueBranch Nothing
eval' (MalList ["if", condition, trueBranch, falseBranch]) = evalIfStmt condition trueBranch (Just falseBranch)
eval' (MalList ("if":_)) = throwSpecialForm "if"

-- fn* special form (lambdas)
eval' (MalList ["fn*", MalList params, body]) = do
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
  pure $
    mkMalTailRecFunction
      body
      params
      currentScope
      (MkMalFunction "lambda" closure False)
eval' (MalList ("fn*":_)) = throwSpecialForm "fn*"

-- quote special form
eval' (MalList ["quote", mt]) = pure mt
eval' (MalList ("quote":_)) = throwSpecialForm "quote"

-- quasiquote special form
eval' (MalList ("quasiquote" : ast)) = quasiquote ast >>= eval'

-- macroexpand
eval' (MalList ["macroexpand", ast]) = asks interpreterScope >>= flip macroexpand ast
eval' (MalList ("macroexpand":_)) = throwSpecialForm "macroexpand"

-- try*/catch*
eval' (MalList ["try*", tryBlock, MalList ["catch*", MalSymbol catchVar, catchBody]]) = do
  currentScope <- asks interpreterScope
  programFilename <- asks interpreterFilename
  liftIO $ eval (Just programFilename) currentScope tryBlock `catches `
      [ Handler $ \(ex :: MalError) -> case ex of
                      UserGeneratedError errVal -> returnError currentScope programFilename errVal
                      _ -> returnError currentScope programFilename (mkMalString $ show ex)
      , Handler $ \(ex :: SomeException) -> returnError currentScope programFilename (mkMalString $ show ex)
      ]
  where
    returnError :: IORef MalScope -> MalFilename -> MalType -> IO MalType
    returnError currentScope programFilename ex = liftIO $ do
        modifyIORef' currentScope (Env.insert catchVar ex)
        eval (Just programFilename) currentScope catchBody
eval' (MalList ("try*":_)) = throwSpecialForm "try*"

eval' xs@(MalList _) = evalAst xs >>= evalCall  -- Here we probably have a function call.
eval' ast            = evalAst ast              -- Otherwise just return the evaluated ast.

-- Functions

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
evalCall (MalList (MalFunction func : args)) = func ^. fBody $ args
evalCall (MalList (MalTailRecFunction (MkMalTailRecFunction body params env func) : args)) = do
  let argBindings = M.fromList $ mkFnBindings (func^.fName) params args
      functionScope = Env.empty {scopeParent = Just env, scopeBindings = argBindings}
  liftIO $ newIORef functionScope >>= flip (eval Nothing) body
evalCall (MalList (x : _)) = liftIO $ throwIO (NotAFunction x)
evalCall ast = pure ast

-- | Bind a list of function names to the corresponding arguments.
-- This handles clojure-style rest params as well.
mkFnBindings :: String -> [MalType] -> [MalType] -> [(String, MalType)]
mkFnBindings functionName = go []
  where
    go :: [(String, MalType)] -> [MalType] -> [MalType] -> [(String, MalType)]
    go bindings ["&", MalSymbol rest] args = (rest, mkMalList args) : bindings
    go _ ("&" : _ : _) _ = throw $ InvalidSignature "expected only 1 argument after '&'"
    go bindings (param@(MalSymbol name) : names) (arg : args)
      | isKeyword arg && (param `compare` arg) == EQ = go ((name, head args) : bindings) names (tail args)
      | otherwise = go ((name, arg) : bindings) names args
    go bindings xs ys
      | length xs == length ys = bindings
      | otherwise = throw $ InvalidSignature ("Mismatched arguments for function: " <> functionName)

-- Macros

-- | 'quasiquote' quasiquotes an expression.
--
-- ...
--
-- Plz don't ask more, I honestly don't know WTF is going on down there.
-- https://github.com/kanaka/mal/blob/master/process/guide.md#step-7-quoting
--
quasiquote :: [MalType] -> Interpreter
quasiquote [MalList ["unquote", ast]] = pure ast
quasiquote [MalList ast] = go ast
    where
        go ((MalList ["splice-unquote", elt]):rest') = do
            result <- go rest'
            -- This assumes that elt will eventually resolve to a list.
            pure $ mkMalList ["concat", elt, result]
        go (elt:ys) = do
            result <- quasiquote [elt]   -- Quasiquote elt
            rest' <- go ys               -- Process the rest
            pure $ mkMalList ["cons", result, rest']
        -- If the ast is empty just return it as is.
        go [] = pure $ mkMalList []
quasiquote [sym@(MalSymbol _)] = pure $ mkMalList ["quote", sym]
quasiquote [m@(MalMap _)] = pure $ mkMalList ["quote", m]
quasiquote [ast]                                                      = pure ast
quasiquote xs = liftIO $ throwIO (InvalidArgs "quasiquote" xs Nothing)

-- | 'isMacroCall' takes the current scope and an AST and returns true if the
-- first element of the AST corresponds to a function that has the @fIsMacro@
-- attribute set to true, or false otherwise.
isMacroCall :: IORef MalScope -> MalType -> IO Bool
isMacroCall currentScope (MalList (MalSymbol sym : _)) = do
  func <- catch (Env.find currentScope sym) (\(_ :: MalError) -> pure mkMalNil)
  case func of
    (MalTailRecFunction tailRec) -> pure $  tailRec ^. tailRecFunction . fIsMacro
    _                            -> pure False
isMacroCall _ _ = pure False

-- | 'macroexpand' takes the current scope and an AST and recursively expands
-- all of its macros. It works like this:
--
-- If AST is a macro call do the following:
--
--  1. Get the corresponding macro function from the environment
--  2. Call the function with the rest of the ast as its arguments
--  3. Recursively call macroexpand on the result
--
-- Else, if AST is not a macro call just return it as is.
macroexpand :: IORef MalScope -> MalType -> Interpreter
macroexpand currentScope ast@(MalList (MalSymbol sym : args)) = do
  isMacro <- liftIO $ isMacroCall currentScope ast
  if not isMacro
    then pure ast
    else do
      (MalTailRecFunction func) <- liftIO $ Env.find currentScope sym
      let macroFunc = func ^. tailRecFunction . fBody
      withScope (func ^. tailRecEnv) $ do
        result <- macroFunc args
        macroexpand currentScope result
macroexpand _ ast = pure ast

-- Helpers

isFalsey :: MalType -> Bool
isFalsey (MalBool False) = True
isFalsey MalNil          = True
isFalsey _               = False

isTruthy :: MalType -> Bool
isTruthy = not . isFalsey

evalIfStmt :: MalType -> MalType -> Maybe MalType -> Interpreter
evalIfStmt condition trueBranch falseBranch = do
  result <- eval' condition
  currentScope <- asks interpreterScope
  fileName <- asks interpreterFilename
  if isTruthy result
    then liftIO $ eval (Just fileName) currentScope trueBranch
    else liftIO $ eval (Just fileName) currentScope (fromMaybe mkMalNil falseBranch)
