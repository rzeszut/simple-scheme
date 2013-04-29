{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Primitives.Equal (equal) where

import Control.Monad
import Scheme.Data
import Scheme.Error
import Scheme.Primitives.Eqv

unpackNum :: SchemeValue -> ThrowsError Integer
unpackNum (Integer i) = return i
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

unpackString :: SchemeValue -> ThrowsError String
unpackString (String s) = return s
unpackString notStr     = throwError $ TypeMismatch "string" notStr

unpackBoolean :: SchemeValue -> ThrowsError Bool
unpackBoolean (Boolean b) = return b
unpackBoolean notBool     = throwError $ TypeMismatch "boolean" notBool

unpackChar :: SchemeValue -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar  = throwError $ TypeMismatch "char" notChar

-- equals
data Unpacker = forall a . Eq a => AnyUnpacker (SchemeValue -> ThrowsError a)

unpackEquals :: SchemeValue -> SchemeValue -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [SchemeValue] -> ThrowsError SchemeValue
equal [c1@(Cons _ _), c2@(Cons _ _)] = equalCons equal c1 c2
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [ AnyUnpacker unpackNum
                     , AnyUnpacker unpackChar
                     , AnyUnpacker unpackString
                     , AnyUnpacker unpackBoolean
                     ]
  eqvEquals <- eqv [arg1, arg2]
  return . Boolean $ (primitiveEquals || let (Boolean x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

equalCons :: ([SchemeValue] -> ThrowsError SchemeValue) -> SchemeValue -> SchemeValue -> ThrowsError SchemeValue
equalCons eq (Cons h1 t1) (Cons h2 t2) = do
    eqh <- equal [h1, h2]
    eqt <- equal [t1, t2]
    return . Boolean $ (let (Boolean h) = eqh;
                            (Boolean t) = eqt
                        in h && t)

