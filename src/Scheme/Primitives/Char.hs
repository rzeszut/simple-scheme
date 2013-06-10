module Scheme.Primitives.Char (charPrimitives) where

import Data.Char
import Scheme.Error
import Scheme.Data
import Scheme.Primitives.Common

charPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
charPrimitives = [ ("char?",            unaryFunction charp)
                 , ("char=?",           charBoolBinop (==))
                 , ("char<?",           charBoolBinop (<))
                 , ("char>?",           charBoolBinop (>))
                 , ("char<=?",          charBoolBinop (<=))
                 , ("char>=?",          charBoolBinop (>=))
                 , ("char-alphabetic?", charAlphap)
                 , ("char-numeric?",    charNump)
                 , ("char-whitespace?", charSpacep)
                 , ("char-upper-case?", charUpperp)
                 , ("char-lower-case?", charLowerp)
                 , ("char->integer",    char2integer)
                 , ("integer->char",    integer2char)
                 , ("char-upcase",      charUpcase)
                 , ("char-downcase",    charDowncase)
                 ]

charp (Char _) = Boolean True
charp _        = Boolean False

charAlphap = charUnary isAlpha (return . Boolean)
charNump   = charUnary isDigit (return . Boolean)
charSpacep = charUnary isSpace (return . Boolean)
charUpperp = charUnary isUpper (return . Boolean)
charLowerp = charUnary isLower (return . Boolean)

char2integer = charUnary (toInteger . ord) (return . Integer)
integer2char = makeUnaryFunction unpackInteger (chr . fromInteger) (return . Char)

charUpcase   = charUnary toUpper (return . Char)
charDowncase = charUnary toLower (return . Char)

charBoolBinop = makeBinaryBoolFunction unpackChar
charUnary     = makeUnaryFunction unpackChar
