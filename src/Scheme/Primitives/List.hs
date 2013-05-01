module Scheme.Primitives.List ( pairp
                              , cons
                              , car
                              , cdr
                              , nullp
                              , listp  
                              ) where

import Scheme.Data
import Lang.Utils.Error

pairp (Cons _ _) = Boolean True
pairp _          = Boolean False

cons :: [SchemeValue] -> ThrowsSchemeError SchemeValue
cons [h, t] = return $ Cons h t
cons args   = throwError $ NumArgs 2 args

car :: SchemeValue -> ThrowsSchemeError SchemeValue
car (Cons h _) = return h
car notPair    = throwError $ TypeMismatch "pair" notPair

cdr :: SchemeValue -> ThrowsSchemeError SchemeValue
cdr (Cons _ t) = return t
cdr notPair    = throwError $ TypeMismatch "pair" notPair

nullp Nil = Boolean True
nullp _   = Boolean False

listp Nil                   = Boolean True
listp (Cons _ Nil)          = Boolean True
listp (Cons _ t@(Cons _ _)) = listp t
listp _                     = Boolean False
