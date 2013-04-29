module Scheme.Eval where

import Scheme.Data
import Scheme.Primitives
import Scheme.Error

fromLispList :: SchemeValue -> [SchemeValue]
fromLispList Nil = []
fromLispList (Cons h t) = h : (fromLispList t)

eval :: SchemeValue -> ThrowsError SchemeValue
eval Nil                                    = return Nil
eval val@(Integer _)                        = return val
eval val@(Float _)                          = return val
eval val@(Rational _)                       = return val
eval val@(Complex _)                        = return val
eval val@(Boolean _)                        = return val
eval val@(Char _)                           = return val
eval val@(String _)                         = return val
eval val@(Vector _)                         = return val

eval (Cons (Symbol "quote") (Cons val Nil)) = return val

-- if expression
eval (Cons (Symbol "if") (Cons pred
                          (Cons conseq
                           (Cons alt Nil)))) =
  do result <- eval pred
     case result of
       Boolean True  -> eval conseq
       Boolean False -> eval alt
       _             -> throwError $ TypeMismatch "boolean" pred

-- cond expression
eval form@(Cons (Symbol "cond") Nil) = throwError $ BadSpecialForm "no true clause in cond expression" form
eval form@(Cons (Symbol "cond") clauses) = evalClauses clauses
  where
    evalClauses (Cons (Cons (Symbol "else")
                       (Cons expr Nil)) _) = eval expr
    evalClauses (Cons (Cons cond
                       (Cons expr Nil)) rest) = do b <- isTrue cond
                                                   if b then eval expr
                                                     else evalClauses rest
    evalClauses form = throwError $ BadSpecialForm "ill-formed cond expression: " form

    isTrue :: SchemeValue -> ThrowsError Bool
    isTrue cond = do
      b <- eval cond
      case b of
        Boolean b -> return b
        _         -> throwError $ TypeMismatch "boolean" cond

-- TODO: case form

-- function application
eval (Cons (Symbol func) args) = mapM eval (fromLispList args)
                                 >>= apply func

eval badForm =
  throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [SchemeValue] -> ThrowsError SchemeValue
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                  ($ args)
                  $ lookup func primitives
