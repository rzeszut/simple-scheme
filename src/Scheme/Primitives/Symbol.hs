module Scheme.Primitives.Symbol (symbolPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

symbolPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
symbolPrimitives = [ ("symbol?",        unaryOp symbolp)
                   , ("symbol->string", unaryThrowingOp symbol2string)
                   , ("string->symbol", unaryThrowingOp string2symbol)
                   ]

symbolp (Symbol _) = Boolean True
symbolp _          = Boolean False

symbol2string :: SchemeValue -> ThrowsSchemeError SchemeValue
symbol2string (Symbol name) = return $ String name
symbol2string notSym        = throwError $ TypeMismatch "symbol" notSym

string2symbol :: SchemeValue -> ThrowsSchemeError SchemeValue
string2symbol (String str) = return $ Symbol str
string2symbol notStr       = throwError $ TypeMismatch "string" notStr
