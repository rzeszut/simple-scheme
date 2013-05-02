module Scheme.Primitives.String (stringPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

stringPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
stringPrimitives = [ ("string?",   unaryOp stringp)
                   , ("string=?",  stringBoolBinop (==))
                   , ("string<?",  stringBoolBinop (<))
                   , ("string>?",  stringBoolBinop (>))
                   , ("string<=?", stringBoolBinop (<=))
                   , ("string>=?", stringBoolBinop (>=))
                   ]

stringp (String _) = Boolean True
stringp _          = Boolean False

stringBoolBinop = boolBinop unpacker
  where
    unpacker (String b) = return b
    unpacker notString  = throwError $ TypeMismatch "string" notString
