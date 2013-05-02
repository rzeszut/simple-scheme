module Scheme.Primitives.List (listPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

listPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
listPrimitives = [ ("pair?",  unaryFunction pairp)
                 , ("cons",   cons)
                 , ("car",    car)
                 , ("cdr",    cdr)
                 , ("null?",  unaryFunction nullp)
                 , ("list?",  unaryFunction listp)
                 ]

pairp (Cons _ _) = Boolean True
pairp _          = Boolean False

cons = makeBinaryFunction return return Cons return

car = makeUnaryFunction unpackPair (\(h, _) -> h) return
cdr = makeUnaryFunction unpackPair (\(_, t) -> t) return

-- set-car!, set-cdr!

nullp Nil = Boolean True
nullp _   = Boolean False

listp Nil                   = Boolean True
listp (Cons _ Nil)          = Boolean True
listp (Cons _ t@(Cons _ _)) = listp t
listp _                     = Boolean False
