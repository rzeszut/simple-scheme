module Scheme.Eval (eval) where

import Scheme.Data
import Scheme.Primitives
import Scheme.Error
import Scheme.Environment

fromLispList :: SchemeValue -> [SchemeValue]
fromLispList Nil        = []
fromLispList (Cons h t) = h : (fromLispList t)

eval :: Environment -> SchemeValue -> IOThrowsError SchemeValue
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
    evalQuasiquoted :: SchemeValue -> IOThrowsError SchemeValue
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

    isTrue :: SchemeValue -> IOThrowsError Bool
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

-- function application
eval env (Cons (Symbol func) args) = mapM (eval env) (fromLispList args)
                                     >>= liftThrows . apply func

eval env badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [SchemeValue] -> ThrowsError SchemeValue
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                  ($ args)
                  $ lookup func primitives
