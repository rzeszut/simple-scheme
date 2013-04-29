module Scheme.Primitives.Eqv (eqv) where

import Scheme.Data
import Scheme.Error

-- TODO: vector support
eqv :: [SchemeValue] -> ThrowsError SchemeValue
eqv [Nil, Nil]                          = return $ Boolean True
eqv [(Symbol arg1), (Symbol arg2)]      = return . Boolean $ arg1 == arg2
eqv [(Integer arg1), (Integer arg2)]    = return . Boolean $ arg1 == arg2
eqv [(Float arg1), (Float arg2)]        = return . Boolean $ arg1 == arg2
eqv [(Rational arg1), (Rational arg2)]  = return . Boolean $ arg1 == arg2
eqv [(Complex arg1), (Complex arg2)]    = return . Boolean $ arg1 == arg2
eqv [(Boolean arg1), (Boolean arg2)]    = return . Boolean $ arg1 == arg2
eqv [(Char arg1), (Char arg2)]          = return . Boolean $ arg1 == arg2
eqv [(String arg1), (String arg2)]      = return . Boolean $ arg1 == arg2
eqv [arg1@(Cons _ _ ), arg2@(Cons _ _)] = return . Boolean $ arg1 `consEqual` arg2
  where
    consEqual Nil Nil = True
    consEqual Nil _   = False
    consEqual _   Nil = False
    consEqual (Cons h1 t1) (Cons h2 t2) = case eqv [h1, h2] of
      Left  _             -> False
      Right (Boolean val) -> val
eqv [_, _]                              = return $ Boolean False
eqv badArgList                          = throwError $ NumArgs 2 badArgList
