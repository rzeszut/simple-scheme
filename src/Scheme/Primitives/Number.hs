{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Scheme.Primitives.Number (numberPrimitives) where

import Control.Monad
import Data.Complex
import Data.Ratio (Rational, numerator, denominator)
import Scheme.Error
import Scheme.Data
import Scheme.Primitives.Common
import Scheme.Primitives.Equal (unpackEquals)
import qualified Scheme.Scanner as S (scan,
                                      Token(getToken),
                                      SchemeToken(Integer,
                                                  Rational,
                                                  Float,
                                                  Complex))

numberPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
numberPrimitives = [ ("number?",   unaryFunction numberp)
                   , ("complex?",  unaryFunction complexp)
                   , ("real?",     unaryFunction realp)
                   , ("rational?", unaryFunction rationalp)
                   , ("integer?",  unaryFunction integerp)

                   , ("=",         equalNum)
                   , ("<",         compareNum (<))
                   , (">",         compareNum (>))
                   , ("<=",        compareNum (<=))
                   , (">=",        compareNum (>=))

                   , ("__add",     numBinop $ num (+))
                   , ("__sub",     numBinop $ num (-))
                   , ("__mul",     numBinop $ num (*))
                   , ("/",         integerBinop div)

                   , ("modulo",    integerBinop mod)
                   , ("quotient",  integerBinop quot)
                   , ("remainder", integerBinop rem)
                     
                   , ("numerator",   numeratorProc)
                   , ("denominator", denominatorProc)

                   , ("floor",    floorProc)
                   , ("ceiling",  ceilingProc)
                   , ("round",    roundProc)
                   , ("truncate", truncateProc)

                   , ("exp",  expProc)
                   , ("log",  logProc)
                   , ("sin",  sinProc)
                   , ("cos",  cosProc)
                   , ("tan",  tanProc)
                   , ("asin", asinProc)
                   , ("acos", acosProc)
                   , ("atan", atanProc)

                   , ("sqrt", sqrtProc)
                   , ("expt", binaryThrowingFunction exptProc)

                   , ("make-rectangular", binaryThrowingFunction makeRectangular)
                   , ("make-polar",       binaryThrowingFunction makePolar)
                   , ("real-part",        realPartProc)
                   , ("imag-part",        imagPartProc)
                   , ("magnitude",        magnitudeProc)
                   , ("angle",            angleProc)

                   , ("num->string", unaryThrowingFunction num2string)
                   , ("string->num", unaryThrowingFunction string2num)
                   ]

numberp (Integer  _) = Boolean True
numberp (Float    _) = Boolean True
numberp (Rational _) = Boolean True
numberp (Complex  _) = Boolean True
numberp _            = Boolean False

complexp = numberp

realp (Float _)    = Boolean True
realp (Integer  _) = Boolean True
realp (Rational _) = Boolean True
realp _            = Boolean False

rationalp (Rational _) = Boolean True
rationalp (Integer _)  = Boolean True
rationalp _            = Boolean False

integerp (Integer _) = Boolean True
integerp _           = Boolean False

numeratorProc   = numberUnaryFunction unpackRational numerator   (return . Integer)
denominatorProc = numberUnaryFunction unpackRational denominator (return . Integer)

floorProc    = numberUnaryFunction unpackFloat (toInteger . floor)    (return . Integer)
ceilingProc  = numberUnaryFunction unpackFloat (toInteger . ceiling)  (return . Integer)
roundProc    = numberUnaryFunction unpackFloat (toInteger . round)    (return . Integer)
truncateProc = numberUnaryFunction unpackFloat (toInteger . truncate) (return . Integer)

expProc  = numberUnaryFunction unpackFloat (exp)  (return . Float)
logProc  = numberUnaryFunction unpackFloat (log)  (return . Float)
sinProc  = numberUnaryFunction unpackFloat (sin)  (return . Float)
cosProc  = numberUnaryFunction unpackFloat (cos)  (return . Float)
tanProc  = numberUnaryFunction unpackFloat (tan)  (return . Float)
asinProc = numberUnaryFunction unpackFloat (asin) (return . Float)
acosProc = numberUnaryFunction unpackFloat (acos) (return . Float)

atanProc :: [SchemeValue] -> ThrowsSchemeError SchemeValue
atanProc [val] = numberUnary unpackFloat (atan) (return . Float) val
atanProc [val1, val2] = do
  num1 <- unpackFloat val1
  num2 <- unpackFloat val2
  return . Float $ atan2 num1 num2
atanProc args = throwError $ NumArgs 2 args

sqrtProc = numberUnaryFunction unpackFloat (sqrt) (return . Float)

exptProc :: SchemeValue -> SchemeValue -> ThrowsSchemeError SchemeValue
exptProc z1 z2 = do
  v1 <- unpackFloat z1
  v2 <- unpackFloat z2
  return . Float $ v1 ** v2

makeRectangular :: SchemeValue -> SchemeValue -> ThrowsSchemeError SchemeValue
makeRectangular z1 z2 = do
  v1 <- unpackFloat z1
  v2 <- unpackFloat z2
  return . Complex $ v1 :+ v2

makePolar :: SchemeValue -> SchemeValue -> ThrowsSchemeError SchemeValue
makePolar z1 z2 = do
  v1 <- unpackFloat z1
  v2 <- unpackFloat z2
  return . Complex $ mkPolar v1 v2

realPartProc  = numberUnaryFunction unpackComplex realPart  (return . Float)
imagPartProc  = numberUnaryFunction unpackComplex imagPart  (return . Float)
magnitudeProc = numberUnaryFunction unpackComplex magnitude (return . Float)
angleProc     = numberUnaryFunction unpackComplex phase     (return . Float)

num2string :: SchemeValue -> ThrowsSchemeError SchemeValue
num2string (Integer i)  = return . String $ show i
num2string (Rational r) = return . String $ show r
num2string (Float f)    = return . String $ show f
num2string (Complex c)  = return . String $ show c
num2string notNum       = throwError $ TypeMismatch "number" notNum

string2num :: SchemeValue -> ThrowsSchemeError SchemeValue
string2num (String str) = token >>= makeNumber
  where
    token = liftM (S.getToken . head) $ liftScanner $ S.scan str
    makeNumber (S.Integer i)  = return $ Integer i
    makeNumber (S.Rational r) = return $ Rational r
    makeNumber (S.Float f)    = return $ Float f
    makeNumber (S.Complex c)  = return $ Complex c
    makeNumber   _            = throwError . Default $ concat ["Not a number: ", str]
string2num notStr = throwError $ TypeMismatch "string" notStr

-- helpers
numberUnaryFunction unpacker function packer =
  unaryThrowingFunction (numberUnary unpacker function packer)
numberUnary :: (SchemeValue -> ThrowsSchemeError a)
               -> (a -> b)
               -> (b -> ThrowsSchemeError SchemeValue)
               -> SchemeValue
               -> ThrowsSchemeError SchemeValue
numberUnary unpacker function packer val = do
  num <- unpacker val
  packer $ function num

integerBinop fun = makeBinaryFunction unpackInteger unpackInteger fun (return . Integer)

-- comparing and equals

data CmpUnpacker = forall a . (Eq a, Ord a) => CmpUnpacker (SchemeValue -> ThrowsSchemeError a)

unpackCompare :: (forall a . (Eq a, Ord a) => a -> a -> Bool)
                 -> SchemeValue
                 -> SchemeValue
                 -> CmpUnpacker
                 -> ThrowsSchemeError Bool
unpackCompare f arg1 arg2 (CmpUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 `f` unpacked2
  `catchError` (const $ return False)

compareNum :: (forall a . (Eq a, Ord a) => a -> a -> Bool)
              -> [SchemeValue]
              -> ThrowsSchemeError SchemeValue
compareNum fun [arg1, arg2] = do
  ret <- liftM or $ mapM (unpackCompare fun arg1 arg2)
         [ CmpUnpacker unpackInteger
         , CmpUnpacker unpackRational
         , CmpUnpacker unpackFloat
         ]
  return $ Boolean ret
compareNum _ args           = throwError $ NumArgs 2 args

equalNum :: [SchemeValue] -> ThrowsSchemeError SchemeValue
equalNum [arg1, arg2] = do
  ret <- liftM or $ mapM (unpackEquals arg1 arg2)
         [ Unpacker unpackInteger
         , Unpacker unpackRational
         , Unpacker unpackFloat
         , Unpacker unpackComplex
         ]
  return $ Boolean ret
equalNum args         = throwError $ NumArgs 2 args

-- bullshit starts here

data Number = forall a . (Num a, N a) => Number a

data NumUnpacker = forall a . (Num a, N a) => NumUnpacker (SchemeValue -> ThrowsSchemeError a)

-- what the fuck
num :: forall a . (Num a, N a) => (a -> a -> a) -> a -> a -> Number
num f x y = Number $ f x y

useNumber :: (Num b, N b) => (forall a . (Num a, N a) => a -> b) -> Number -> b
useNumber f (Number n) = f n

-- what the fuck is this
unpackBinop :: (forall a . (Num a, N a) => a -> a -> Number)
               -> SchemeValue
               -> SchemeValue
               -> NumUnpacker
               -> ThrowsSchemeError Number
unpackBinop f arg1 arg2 (NumUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 `f` unpacked2
  `catchError` (const . throwError $ Default "invalid number")
  
 -- what the fuck is that
numBinop :: (forall a . (Num a, N a) => a -> a -> Number)
            -> [SchemeValue]
            -> ThrowsSchemeError SchemeValue
numBinop fun [arg1, arg2] = do
  ret <- liftM head $ sequence $ filterErrors $ map (unpackBinop fun arg1 arg2)
         [ NumUnpacker unpackInteger
         , NumUnpacker unpackRational
         , NumUnpacker unpackFloat
         , NumUnpacker unpackComplex
         ]
  return $ case ret of { Number n -> makeNumber n }
  where
    filterErrors = filter (\x -> case x of
                              Left _  -> False
                              Right _ -> True)
numBinop _ args           = throwError $ NumArgs 2 args

{-
-- I don't even
unpackBinop :: (forall a . (Num a, N a) => a -> a -> Number)
               -> [SchemeValue]
               -> NumUnpacker
               -> ThrowsSchemeError Number
unpackBinop f args (NumUnpacker unpacker) =
  do unpacked <- mapM unpacker args
     return $ foldl (\x acc -> useNumber (flip f x) acc ) (Number $ head unpacked) (tail unpacked)
  `catchError` (const . throwError $ Default "invalid number")

numBinop :: (forall a . (Num a, N a) => Number -> a -> Number)
            -> [SchemeValue]
            -> ThrowsSchemeError SchemeValue
numBinop fun args = do
  ret <- liftM head $ sequence $ filterErrors $ map (unpackBinop fun args)
         [ NumUnpacker unpackInteger
         , NumUnpacker unpackRational
         , NumUnpacker unpackFloat
         , NumUnpacker unpackComplex
         ]
  return $ case ret of { Number n -> makeNumber n }
  where
    filterErrors = filter (\x -> case x of
                              Left _  -> False
                              Right _ -> True)
numBinop _ args           = throwError $ NumArgs 2 args
-}

class N a where
  makeNumber :: a -> SchemeValue

instance N Integer where
  makeNumber = Integer

instance N Rational where
  makeNumber = Rational

instance N Double where
  makeNumber = Float

instance N (Complex Double) where
  makeNumber = Complex
