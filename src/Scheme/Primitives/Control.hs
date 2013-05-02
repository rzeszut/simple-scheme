module Scheme.Primitives.Control (controlPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Eval (apply)
import Scheme.Primitives.Common

controlPrimitives :: [(String, [SchemeValue] -> IOThrowsSchemeError SchemeValue)]
controlPrimitives = [ ("apply",      applyProc)
                    , ("error",      \x -> liftThrows $ unaryThrowingOp errorProc x)
                    , ("procedure?", \x -> liftThrows $ unaryOp procedurep x)
                    ]

applyProc :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
applyProc [fun, args] = apply fun (fromLispList args)

errorProc :: SchemeValue -> ThrowsSchemeError SchemeValue
errorProc (String message) = throwError $ Default message

procedurep (NativeFunction _)   = Boolean True
procedurep (IONativeFunction _) = Boolean True
procedurep (Function _ _ _ _)   = Boolean True
procedurep _                    = Boolean False
