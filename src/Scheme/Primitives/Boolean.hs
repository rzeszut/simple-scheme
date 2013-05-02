module Scheme.Primitives.Boolean (booleanPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

booleanPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
booleanPrimitives = [ ("boolean?", unaryOp booleanp)
                    , ("not",      unaryOp notProc)
                    , ("&&",       booleanBoolBinop (&&))
                    , ("||",       booleanBoolBinop (||))
                    ]

notProc (Boolean True)  = Boolean False
notProc (Boolean False) = Boolean True
notProc _               = Boolean False

booleanp (Boolean _) = Boolean True
booleanp _           = Boolean False

-- TODO: will this work with and and or?
booleanBoolBinop = boolBinop unpacker
  where
    unpacker (Boolean b) = return b
    unpacker notBool     = throwError $ TypeMismatch "boolean" notBool
