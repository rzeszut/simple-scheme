module Scheme.Primitives.Common ( boolBinop
                                , unaryOp
                                , unaryThrowingOp
                                ) where

import Lang.Utils.Error
import Scheme.Data

boolBinop :: (SchemeValue -> ThrowsSchemeError a) -> (a -> a -> Bool) -> [SchemeValue] -> ThrowsSchemeError SchemeValue
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise       = do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return . Boolean $ left `op` right

unaryOp :: (SchemeValue -> SchemeValue) -> [SchemeValue] -> ThrowsSchemeError SchemeValue
unaryOp f [v]  = return $ f v
unaryOp _ vals = throwError $ NumArgs 1 vals

unaryThrowingOp :: (SchemeValue -> ThrowsSchemeError SchemeValue) -> [SchemeValue] -> ThrowsSchemeError SchemeValue
unaryThrowingOp f [v]  = f v
unaryThrowingOp _ vals = throwError $ NumArgs 1 vals

