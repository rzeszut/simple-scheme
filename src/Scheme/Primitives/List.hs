module Scheme.Primitives.List (listPrimitives) where

import Scheme.Error
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

pairp (DottedList _ _) = Boolean True
pairp (List [])        = Boolean False
pairp (List _)         = Boolean True
pairp _                = Boolean False

cons = makeBinaryFunction return return cons' return
  where
    cons' x (List [])             = List [x]
    cons' x (List xs)             = List $ x : xs
    cons' x (DottedList xs xlast) = DottedList (x : xs) xlast
    cons' x y                     = DottedList [x] y

car = makeUnaryFunction unpackPair (\(h, _) -> h) return
cdr = makeUnaryFunction unpackPair (\(_, t) -> t) return

-- set-car!, set-cdr!

nullp (List []) = Boolean True
nullp _         = Boolean False

listp (List _) = Boolean True
listp _        = Boolean False
