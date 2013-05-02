module Scheme.Eval (eval, apply) where

import Control.Monad (liftM, mapM)
import Control.Monad.Trans (liftIO)
import Scheme.Data
import Scheme.Parser (load)
import Lang.Utils.Error
import Lang.Utils.Environment (getVar, setVar, defineVar, bindVars)

eval :: SchemeEnvironment -> SchemeValue -> IOThrowsSchemeError SchemeValue
eval _ Nil              = return Nil
eval _ val@(Integer _)  = return val
eval _ val@(Float _)    = return val
eval _ val@(Rational _) = return val
eval _ val@(Complex _)  = return val
eval _ val@(Boolean _)  = return val
eval _ val@(Char _)     = return val
eval _ val@(String _)   = return val
eval _ val@(Vector _)   = return val
eval env (Symbol id)    = getVar env id

eval _ (Cons (Symbol "quote") (Cons val Nil))        = return val
eval _ form@(Cons (Symbol "unquote") (Cons val Nil)) = throwError $ BadSpecialForm "expression invalid outside of quasiquote" form
eval env (Cons (Symbol "quasiquote") (Cons val Nil)) = evalQuasiquoted val
  where
    evalQuasiquoted :: SchemeValue -> IOThrowsSchemeError SchemeValue
    evalQuasiquoted (Cons (Symbol "unquote") (Cons val Nil)) = eval env val
    evalQuasiquoted (Cons car cdr) =
      do head <- evalQuasiquoted car
         tail <- evalQuasiquoted cdr
         return $ Cons head tail
    evalQuasiquoted val = return val

-- if expression
eval env (Cons (Symbol "if") (Cons pred
                              (Cons conseq
                               (Cons alt Nil)))) =
  do result <- eval env pred
     case result of
       Boolean True  -> eval env conseq
       Boolean False -> eval env alt
       _             -> throwError $ TypeMismatch "boolean" pred

-- cond expression
eval _ form@(Cons (Symbol "cond") Nil) = throwError $ BadSpecialForm "no true clause in cond expression" form
eval env form@(Cons (Symbol "cond") clauses) = evalClauses clauses
  where
    evalClauses (Cons (Cons (Symbol "else")
                       (Cons expr Nil)) _) = eval env expr
    evalClauses (Cons (Cons cond
                       (Cons expr Nil)) rest) = do b <- isTrue cond
                                                   if b then eval env expr
                                                     else evalClauses rest
    evalClauses form = throwError $ BadSpecialForm "ill-formed cond expression: " form

    isTrue :: SchemeValue -> IOThrowsSchemeError Bool
    isTrue cond = do
      b <- eval env cond
      case b of
        Boolean b -> return b
        _         -> throwError $ TypeMismatch "boolean" cond

-- TODO: case form

-- set!
eval env (Cons (Symbol "set!") (Cons (Symbol var) (Cons form Nil))) =
  eval env form >>= setVar env var

-- define
eval env (Cons (Symbol "define") (Cons (Symbol var) (Cons form Nil))) =
  eval env form >>= defineVar env var

eval env (Cons (Symbol "define") (Cons (Cons (Symbol var) paramsList) body)) =
  makeFunction vararg env params body >>= defineVar env var
  where
    (params, v) = fromDottedLispList paramsList
    vararg      = case v of
      Nil -> Nothing
      x   -> Just $ show x

-- lambda
eval env (Cons (Symbol "lambda") (Cons paramsList body)) = 
  makeFunction vararg env params body
  where
    (params, v) = fromDottedLispList paramsList
    vararg      = case v of
      Nil -> Nothing
      x   -> Just $ show x

-- load
eval env (Cons (Symbol "load") (Cons (String filename) Nil)) =
  load filename >>= liftM last . mapM (eval env)

-- begin
eval env (Cons (Symbol "begin") body) =
  liftM last $ mapM (eval env) (fromLispList body)

-- function application
eval env (Cons (Symbol function) args) = do
  fun     <- getVar env function
  argVals <- mapM (eval env) (fromLispList args)
  apply fun argVals

eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm
  
makeFunction varargs env params body = return $ Function (map show params) varargs body env

-- TODO: add environment for native functions
apply :: SchemeValue -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
apply (NativeFunction fun) args   = liftThrows $ fun args
apply (IONativeFunction fun) args = fun args
apply (Function params vararg body closure) args =
  if num params /= num args && vararg == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArg vararg >>= evalBody
  where
    remainingArgs      = drop (length params) args
    num                = toInteger . length
    evalBody env       = liftM last $ mapM (eval env) (fromLispList body)
    bindVarArg arg env = case arg of
      Nothing      -> return env
      Just argName -> liftIO $ bindVars env [(argName, toLispList remainingArgs)]
apply fun _ = throwError $ NotFunction "value is not a function" fun
