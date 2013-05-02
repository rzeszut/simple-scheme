module Scheme.Primitives.Boolean (booleanPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

booleanPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
booleanPrimitives = [ ("boolean?", unaryFunction booleanp)
                    , ("not",      unaryFunction notProc)
                    , ("&&",       booleanBoolBinop (&&))
                    , ("||",       booleanBoolBinop (||))
                    ]

notProc (Boolean True)  = Boolean False
notProc (Boolean False) = Boolean True
notProc _               = Boolean False

booleanp (Boolean _) = Boolean True
booleanp _           = Boolean False

-- TODO: will this work with and and or?
booleanBoolBinop = makeBinaryBoolFunction unpackBoolean
