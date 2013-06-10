module Scheme.Primitives.Symbol (symbolPrimitives) where

import Scheme.Error
import Scheme.Data
import Scheme.Primitives.Common

symbolPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
symbolPrimitives = [ ("symbol?",        unaryFunction symbolp)
                   , ("symbol->string", symbol2string)
                   , ("string->symbol", string2symbol)
                   ]

symbolp (Symbol _) = Boolean True
symbolp _          = Boolean False

symbol2string = makeUnaryFunction unpackSymbol String return
string2symbol = makeUnaryFunction unpackString Symbol return
