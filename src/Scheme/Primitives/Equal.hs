{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Primitives.Equal (equalPrimitives) where

import Control.Monad
import Scheme.Data
import Lang.Utils.Error

equalPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
equalPrimitives = [ ("eqv?", eqv)
                  , ("eq?",  eqv)
                  , ("equal?", equal)
                  ]

-- TODO: vector support
eqv :: [SchemeValue] -> ThrowsSchemeError SchemeValue
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

unpackNum :: SchemeValue -> ThrowsSchemeError Integer
unpackNum (Integer i) = return i
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

unpackString :: SchemeValue -> ThrowsSchemeError String
unpackString (String s) = return s
unpackString notStr     = throwError $ TypeMismatch "string" notStr

unpackBoolean :: SchemeValue -> ThrowsSchemeError Bool
unpackBoolean (Boolean b) = return b
unpackBoolean notBool     = throwError $ TypeMismatch "boolean" notBool

unpackChar :: SchemeValue -> ThrowsSchemeError Char
unpackChar (Char c) = return c
unpackChar notChar  = throwError $ TypeMismatch "char" notChar

-- equals
data Unpacker = forall a . Eq a => AnyUnpacker (SchemeValue -> ThrowsSchemeError a)

unpackEquals :: SchemeValue -> SchemeValue -> Unpacker -> ThrowsSchemeError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [SchemeValue] -> ThrowsSchemeError SchemeValue
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

equalCons :: ([SchemeValue] -> ThrowsSchemeError SchemeValue) -> SchemeValue -> SchemeValue -> ThrowsSchemeError SchemeValue
equalCons eq (Cons h1 t1) (Cons h2 t2) = do
    eqh <- equal [h1, h2]
    eqt <- equal [t1, t2]
    return . Boolean $ (let (Boolean h) = eqh;
                            (Boolean t) = eqt
                        in h && t)

