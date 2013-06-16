module Scheme.Primitives.Control (controlPrimitives) where

import Control.Monad.Trans (liftIO)
import Scheme.Error
import Scheme.Data
import Scheme.Eval (apply)
import Scheme.Environment (isBound)
import Scheme.Primitives.Common

controlPrimitives :: [(String, SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue)]
controlPrimitives = [ ("apply",      applyProc)
                    , ("error",      \_ args -> liftThrows $ unaryThrowingFunction errorProc args)
                    , ("procedure?", \_ args -> liftThrows $ unaryFunction procedurep args)
                    , ("bound?",     boundp)
                    ]

applyProc :: SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
applyProc env [fun, List args] = apply env fun args
applyProc env (fun : args)     = apply env fun args

errorProc :: SchemeValue -> ThrowsSchemeError SchemeValue
errorProc (String message) = throwError $ Default message

procedurep (NativeFunction _)   = Boolean True
procedurep (IONativeFunction _) = Boolean True
procedurep (Function _ _ _ _)   = Boolean True
procedurep _                    = Boolean False

boundp :: SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
boundp env [(Symbol var)] = (liftIO $ isBound env var) >>= return . Boolean
boundp env [(String var)] = (liftIO $ isBound env var) >>= return . Boolean
boundp _ [notSymbol]      = throwError $ TypeMismatch "symbol" notSymbol
boundp _ args             = throwError $ NumArgs 1 args
