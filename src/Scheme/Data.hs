module Scheme.Data where

import Data.Array
import Data.Ratio
import Data.Complex

data SchemeValue = Nil
                 | Cons SchemeValue SchemeValue
                 | Symbol String
                 | Integer Integer
                 | Float Double
                 | Rational Rational
                 | Complex (Complex Double)
                 | Boolean Bool
                 | Char Char
                 | String String
                 | Vector (Array Int SchemeValue)

showVal :: SchemeValue -> String
showVal Nil                  = "()"
showVal (Symbol    name)     = name
showVal (Integer   int)      = show int
showVal (Float     dbl)      = show dbl
showVal (Rational  ratio)    = concat [ show $ numerator ratio
                                      , "/"
                                      , show $ denominator ratio]
showVal (Complex   (r :+ i)) = concat [show r, "+", show i, "i"]
showVal (Boolean   True)     = "#t"
showVal (Boolean   False)    = "#f"
showVal (Char c)             = "#\\" ++ [c]
showVal (String    str)      = concat ["\"", str, "\""]
showVal (Vector    a)        = concat ["#(", contents, ")"]
  where
    contents = unwords $ map showVal $ elems a
showVal cell@(Cons _ _)      = concat ["(", showCell cell, ")"]
  where
    showCell Nil                       = ""
    showCell (Cons car Nil)            = showVal car
    showCell (Cons car cdr@(Cons _ _)) = concat [showVal car, " ", showCell cdr]
    showCell (Cons car cdr)            = concat [showVal car, " . ", showVal cdr]

instance Show SchemeValue where
  show = showVal
