module Scheme.Primitives.Number (numberPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

numberPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
numberPrimitives = [ ("number?",   unaryFunction numberp)
                   , ("complex?",  unaryFunction complexp)
                   , ("real?",     unaryFunction realp)
                   , ("rational?", unaryFunction rationalp)
                   , ("integer?",  unaryFunction integerp)
                   , ("+",         numericBinop (+))
                   , ("-",         numericBinop (-))
                   , ("*",         numericBinop (*))
                   , ("/",         numericBinop div)
                   , ("mod",       numericBinop mod)
                   , ("quotient",  numericBinop quot)
                   , ("remainder", numericBinop rem)
                   , ("=",         numBoolBinop (==))
                   , ("<",         numBoolBinop (<))
                   , (">",         numBoolBinop (>))
                   , ("<=",        numBoolBinop (<=))
                   , (">=",        numBoolBinop (>=))
                   ]

numberp (Integer  _) = Boolean True
numberp (Float    _) = Boolean True
numberp (Rational _) = Boolean True
numberp (Complex  _) = Boolean True
numberp _            = Boolean False

complexp (Complex _) = Boolean True
complexp _           = Boolean False

realp (Float _) = Boolean True
realp _         = Boolean False

rationalp (Rational _) = Boolean True
rationalp _            = Boolean False

integerp (Integer _) = Boolean True
integerp _           = Boolean False

-- TODO: proper unpackers
-- these unpackers are used by boolBinop AND equal?
-- which is rather wrong, because equal? unpackers should be really lax
-- e.g. (equals? "2" 2.0) --> #t
-- but (string= "2" 2.0) should throw a type mismatch error
-- therefore, there should be two separate sets of unpackers

-- use Haskell Num class (?)
unpackNum :: SchemeValue -> ThrowsSchemeError Integer
unpackNum (Integer i) = return i
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

numericBinop :: (Integer -> Integer -> Integer)
                -> [SchemeValue]
                -> ThrowsSchemeError SchemeValue
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params
                                >>= return . Integer . foldl1 op

numBoolBinop     = makeBinaryBoolFunction unpackNum
