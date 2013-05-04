{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Primitives.Common (
  -- * Functions makers
  makeUnaryFunction
  , makeBinaryFunction
  , makeBinaryBoolFunction
  , unaryFunction
  , unaryThrowingFunction
    -- * Unpackers
  , Unpacker(..)
    -- ** Simple unpackers
  , unpackInteger
  , unpackRational
  , unpackFloat
  , unpackComplex
  , unpackString
  , unpackBoolean
  , unpackChar
  , unpackPair
  , unpackSymbol
  ) where

import Data.Complex (Complex((:+)))
import Data.Ratio (Rational, (%), numerator, denominator)
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

--
-- General unpackers
--
data Unpacker = forall a . Eq a => Unpacker (SchemeValue -> ThrowsSchemeError a)

unpackInteger :: SchemeValue -> ThrowsSchemeError Integer
unpackInteger (Integer i) = return i
unpackInteger notInteger  = throwError $ TypeMismatch "integer" notInteger

unpackRational :: SchemeValue -> ThrowsSchemeError Rational
unpackRational (Integer i)  = return $ i % 1
unpackRational (Rational r) = return r
unpackRational notRational  = throwError $ TypeMismatch "rational" notRational

unpackFloat :: SchemeValue -> ThrowsSchemeError Double
unpackFloat (Integer i)  = return $ fromInteger i
unpackFloat (Rational r) = return $ (fromIntegral $ numerator r)
                           / (fromIntegral $ denominator r)
unpackFloat (Float f)    = return f
unpackFloat notFloat     = throwError $ TypeMismatch "float" notFloat

unpackComplex :: SchemeValue -> ThrowsSchemeError (Complex Double)
unpackComplex (Integer i)  = return $ (fromInteger i) :+ 0
unpackComplex (Rational r) = return $ ((fromIntegral $ numerator r)
                                       / (fromIntegral $ denominator r)) :+ 0
unpackComplex (Float f)    = return $ f :+ 0
unpackComplex (Complex c)  = return c
unpackComplex notComplex   = throwError $ TypeMismatch "complex" notComplex

unpackString :: SchemeValue -> ThrowsSchemeError String
unpackString (String s) = return s
unpackString notStr     = throwError $ TypeMismatch "string" notStr

unpackBoolean :: SchemeValue -> ThrowsSchemeError Bool
unpackBoolean (Boolean b) = return b
unpackBoolean _           = return True -- every value other than #f is treated as true
--unpackBoolean notBool     = throwError $ TypeMismatch "boolean" notBool

unpackChar :: SchemeValue -> ThrowsSchemeError Char
unpackChar (Char c) = return c
unpackChar notChar  = throwError $ TypeMismatch "char" notChar

unpackPair :: SchemeValue -> ThrowsSchemeError (SchemeValue, SchemeValue)
unpackPair (Cons car cdr) = return (car, cdr)
unpackPair notPair        = throwError $ TypeMismatch "pair" notPair

unpackSymbol :: SchemeValue -> ThrowsSchemeError String
unpackSymbol (Symbol s) = return s
unpackSymbol notSym     = throwError $ TypeMismatch "symbol" notSym
