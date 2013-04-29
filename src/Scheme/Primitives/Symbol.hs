module Scheme.Primitives.Symbol where

import Scheme.Data
import Scheme.Error

symbolp (Symbol _) = Boolean True
symbolp _          = Boolean False

symbol2string :: SchemeValue -> ThrowsError SchemeValue
symbol2string (Symbol name) = return $ String name
symbol2string notSym        = throwError $ TypeMismatch "symbol" notSym

string2symbol :: SchemeValue -> ThrowsError SchemeValue
string2symbol (String str) = return $ Symbol str
string2symbol notStr       = throwError $ TypeMismatch "string" notStr
