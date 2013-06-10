{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Primitives.Equal ( equalPrimitives
                               , unpackEquals
                               ) where

import Control.Monad
import Scheme.Data
import Scheme.Primitives.Common
import Scheme.Error

equalPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
equalPrimitives = [ ("eqv?", eqv)
                  , ("eq?",  eqv)
                  , ("equal?", equal)
                  ]

-- TODO: vector support; no list, vector, function equality
eqv :: [SchemeValue] -> ThrowsSchemeError SchemeValue
eqv [(Symbol arg1),     (Symbol arg2)]     = return . Boolean $ arg1 == arg2
eqv [(Integer arg1),    (Integer arg2)]    = return . Boolean $ arg1 == arg2
eqv [(Float arg1),      (Float arg2)]      = return . Boolean $ arg1 == arg2
eqv [(Rational arg1),   (Rational arg2)]   = return . Boolean $ arg1 == arg2
eqv [(Complex arg1),    (Complex arg2)]    = return . Boolean $ arg1 == arg2
eqv [(Boolean arg1),    (Boolean arg2)]    = return . Boolean $ arg1 == arg2
eqv [(Char arg1),       (Char arg2)]       = return . Boolean $ arg1 == arg2
eqv [(String arg1),     (String arg2)]     = return . Boolean $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ x : xs, List $ y : ys]
eqv [(List xs),         (List ys)]         = listEqual eqv xs ys
eqv [_, _]                                 = return $ Boolean False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

listEqual :: ([SchemeValue] -> ThrowsSchemeError SchemeValue)
             -> [SchemeValue]
             -> [SchemeValue]
             -> ThrowsSchemeError SchemeValue
listEqual eq xs ys =
  return . Boolean $ (length xs == length ys) && (all eqvPair $ zip xs ys)
  where
    eqvPair (x1, x2) = case eq [x1, x2] of
      Left _              -> False
      Right (Boolean val) -> val
  
-- equals
unpackEquals :: SchemeValue -> SchemeValue -> Unpacker -> ThrowsSchemeError Bool
unpackEquals arg1 arg2 (Unpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

equal :: [SchemeValue] -> ThrowsSchemeError SchemeValue
equal [(List xs),         (List ys)]         = listEqual equal xs ys
equal [(DottedList xs x), (DottedList ys y)] = listEqual equal (x : xs) (y : ys)
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [ Unpacker unpackInteger
                     , Unpacker unpackRational
                     , Unpacker unpackFloat
                     , Unpacker unpackComplex
                     , Unpacker unpackChar
                     , Unpacker unpackString
                     , Unpacker unpackBoolean'
                     ]
  eqvEquals <- eqv [arg1, arg2]
  return . Boolean $ (primitiveEquals || let (Boolean x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- equal? must use different boolean unpacker, the standard one breaks this function
unpackBoolean' :: SchemeValue -> ThrowsSchemeError Bool
unpackBoolean' (Boolean b) = return b
unpackBoolean' notBool     = throwError $ TypeMismatch "boolean" notBool
