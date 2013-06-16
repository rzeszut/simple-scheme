module Scheme.Primitives.Vector (vectorPrimitives
                                , vectorIoPrimitives
                                ) where

import Control.Monad.Trans (liftIO)
import Data.Array (elems, (!), bounds)
import Data.IORef (readIORef, writeIORef)
import Scheme.Data
import Scheme.Error
import Scheme.Primitives.Common

vectorPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
vectorPrimitives = [ ("vector?",       unaryFunction vectorp)
                   , ("vector-length", unaryFunction vectorLength)
                   ]

vectorIoPrimitives :: [(String, SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue)]
vectorIoPrimitives = [ ("make-vector",  ignoreEnvironment makeVectorProc)
                     , ("vector-ref",   ignoreEnvironment vectorRef)
                     , ("vector-set!",  ignoreEnvironment vectorSet)
                     , ("vector->list", ignoreEnvironment $ unaryIoThrowingFunction vector2list)
                     , ("list->vector", ignoreEnvironment $ unaryIoThrowingFunction list2vector)
                     ]

vectorp (Vector _) = Boolean True
vectorp _          = Boolean False

vectorLength :: SchemeValue -> SchemeValue
vectorLength (Vector v) = let (_, l) = bounds v in Integer $ toInteger (l + 1)

makeVectorProc :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
makeVectorProc [(Integer k)]       = makeVectorProc [(Integer k), (Boolean False)]
makeVectorProc [(Integer k), fill] = makeVector $ take (fromInteger k) $ repeat fill
makeVectorProc (notInt : _)        = throwError $ TypeMismatch "integer" notInt
makeVectorProc args                = throwError $ NumArgs 2 args

vectorRef :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
vectorRef [(Vector v), (Integer i)] = 
  if not $ inBounds (fromInteger i) (bounds v) then throwError $ Default "Index not in bounds."
  else do
    val <- liftIO $ readIORef $ v ! (fromInteger i)
    return val
vectorRef [notVec, (Integer _)]    = throwError $ TypeMismatch "vector" notVec
vectorRef [_, notInt]              = throwError $ TypeMismatch "integer" notInt
vectorRef args                     = throwError $ NumArgs 2 args

vectorSet :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
vectorSet [(Vector v), (Integer i), val] =
  if not $ inBounds (fromInteger i) (bounds v) then throwError $ Default "Index not in bounds."
  else do
    liftIO $ writeIORef (v ! (fromInteger i)) val
    return val
vectorSet [notVec, (Integer _), _]       = throwError $ TypeMismatch "vector" notVec
vectorSet [_, notInt, _]                 = throwError $ TypeMismatch "integer" notInt
vectorSet args                           = throwError $ NumArgs 3 args
  
inBounds i (beg, end) = beg <= i && i <= end

vector2list :: SchemeValue -> IOThrowsSchemeError SchemeValue
vector2list (Vector v) = do
  vals <- liftIO $ sequence $ map readIORef $ elems v
  return $ List vals
vector2list notVector  = throwError $ TypeMismatch "vector" notVector

list2vector :: SchemeValue -> IOThrowsSchemeError SchemeValue
list2vector (List lst) = makeVector lst
list2vector notList    = throwError $ TypeMismatch "list" notList
