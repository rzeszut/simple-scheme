-- | Module contains definition of Scheme datatypes and errors.
module Scheme.Data (
  -- * Data types
  -- ** Scheme Values
  SchemeValue(..)
  -- ** Scheme Errors
  , SchemeError(..)
  , ThrowsSchemeError(..)
  , IOThrowsSchemeError(..)
    -- ** Scheme Environment
  , SchemeEnvironment(..)
    -- * Functions
  ) where

import Data.Array (Array, elems)
import Data.Ratio (Rational, numerator, denominator)
import Data.Complex (Complex((:+)))
import Text.ParserCombinators.Parsec (ParseError)
import Lang.Utils.Error (LangError(..), ThrowsError, IOThrowsError)
import Lang.Utils.Environment (Environment)

-- | Scheme value datatype
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
                 | NativeFunction ([SchemeValue] -> ThrowsSchemeError SchemeValue)
                 | Function { params  :: [String]
                            , vararg  :: Maybe String
                            , body    :: SchemeValue
                            , closure :: Environment SchemeValue
                            }

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
showVal (Char      c)        = "#\\" ++ [c]
showVal (String    str)      = concat ["\"", str, "\""]
showVal (Vector    a)        = concat ["#(", contents, ")"]
  where
    contents = unwords $ map showVal $ elems a

showVal (NativeFunction _)   = "<native function>"
showVal (Function {params = args, vararg = varargs}) =
  "(lambda (" ++ unwords args ++
  (case varargs of
      Nothing  -> ""
      Just arg -> " . " ++ arg) ++ ") ... )"

showVal cell@(Cons _ _)      = concat ["(", showCell cell, ")"]
  where
    showCell Nil                       = ""
    showCell (Cons car Nil)            = showVal car
    showCell (Cons car cdr@(Cons _ _)) = concat [showVal car, " ", showCell cdr]
    showCell (Cons car cdr)            = concat [showVal car, " . ", showVal cdr]

instance Show SchemeValue where
  show = showVal

type SchemeError         = LangError SchemeValue
type ThrowsSchemeError   = ThrowsError SchemeValue
type IOThrowsSchemeError = IOThrowsError SchemeValue

type SchemeEnvironment   = Environment SchemeValue
