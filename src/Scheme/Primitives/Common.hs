module Scheme.Primitives.Common (
  -- * Functions
  -- ** Function makers
  makeUnaryFunction
  , makeBinaryFunction
  , makeBinaryBoolFunction
  , unaryFunction
  , unaryThrowingFunction
    -- * Simple unpackers
  , unpackInteger
  , unpackString
  , unpackBoolean
  , unpackChar
  , unpackPair
  , unpackSymbol
  ) where

import Lang.Utils.Error
import Scheme.Data

makeUnaryFunction :: (SchemeValue -> ThrowsSchemeError b)
                     -> (b -> a)
                     -> (a -> ThrowsSchemeError SchemeValue)
                     -> [SchemeValue]
                     -> ThrowsSchemeError SchemeValue
makeUnaryFunction unpacker fun constructor [c]  = unpacker c >>= constructor . fun
makeUnaryFunction _        _   _           vals = throwError $ NumArgs 1 vals

makeBinaryFunction :: (SchemeValue -> ThrowsSchemeError a)
                      -> (SchemeValue -> ThrowsSchemeError b)
                      -> (a -> b -> c)
                      -> (c -> ThrowsSchemeError SchemeValue)
                      -> [SchemeValue]
                      -> ThrowsSchemeError SchemeValue
makeBinaryFunction unpacker1 unpacker2 op constructor args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise       = do
    left  <- unpacker1 $ args !! 0
    right <- unpacker2 $ args !! 1
    constructor $ left `op` right

makeBinaryBoolFunction :: (SchemeValue -> ThrowsSchemeError a)
                          -> (a -> a -> Bool)
                          -> [SchemeValue]
                          -> ThrowsSchemeError SchemeValue
makeBinaryBoolFunction unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise       = do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return . Boolean $ left `op` right

unaryFunction :: (SchemeValue -> SchemeValue)
                 -> [SchemeValue]
                 -> ThrowsSchemeError SchemeValue
unaryFunction f [v]  = return $ f v
unaryFunction _ vals = throwError $ NumArgs 1 vals

unaryThrowingFunction :: (SchemeValue -> ThrowsSchemeError SchemeValue)
                         -> [SchemeValue]
                         -> ThrowsSchemeError SchemeValue
unaryThrowingFunction f [v]  = f v
unaryThrowingFunction _ vals = throwError $ NumArgs 1 vals

unpackInteger :: SchemeValue -> ThrowsSchemeError Integer
unpackInteger (Integer i) = return i
unpackInteger notInteger  = throwError $ TypeMismatch "integer" notInteger

unpackString :: SchemeValue -> ThrowsSchemeError String
unpackString (String s) = return s
unpackString notStr     = throwError $ TypeMismatch "string" notStr

unpackBoolean :: SchemeValue -> ThrowsSchemeError Bool
unpackBoolean (Boolean b) = return b
unpackBoolean notBool     = throwError $ TypeMismatch "boolean" notBool

unpackChar :: SchemeValue -> ThrowsSchemeError Char
unpackChar (Char c) = return c
unpackChar notChar  = throwError $ TypeMismatch "char" notChar

unpackPair :: SchemeValue -> ThrowsSchemeError (SchemeValue, SchemeValue)
unpackPair (Cons car cdr) = return (car, cdr)
unpackPair notPair        = throwError $ TypeMismatch "pair" notPair

unpackSymbol :: SchemeValue -> ThrowsSchemeError String
unpackSymbol (Symbol s) = return s
unpackSymbol notSym     = throwError $ TypeMismatch "symbol" notSym
