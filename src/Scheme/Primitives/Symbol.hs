module Scheme.Primitives.Symbol ( symbolp
                                , symbol2string
                                , string2symbol
                                ) where

import Scheme.Data
import Lang.Utils.Error

symbolp (Symbol _) = Boolean True
symbolp _          = Boolean False

symbol2string :: SchemeValue -> ThrowsSchemeError SchemeValue
symbol2string (Symbol name) = return $ String name
symbol2string notSym        = throwError $ TypeMismatch "symbol" notSym

string2symbol :: SchemeValue -> ThrowsSchemeError SchemeValue
string2symbol (String str) = return $ Symbol str
string2symbol notStr       = throwError $ TypeMismatch "string" notStr
