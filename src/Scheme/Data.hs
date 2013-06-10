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
  ) where

import Data.Array (Array, elems)
import Data.Ratio (Rational, numerator, denominator)
import Data.Complex (Complex((:+)))
import Scheme.Error (LangError(..), ThrowsError, IOThrowsError)
import Scheme.Environment (Environment)
import Scheme.Parser (Command(..))
import System.IO (Handle)

-- | Scheme value datatype
data SchemeValue = List [SchemeValue]
                 | DottedList [SchemeValue] SchemeValue
                 | Symbol String
                 | Integer Integer
                 | Rational Rational
                 | Float Double
                 | Complex (Complex Double)
                 | Boolean Bool
                 | Char Char
                 | String String
                 | Vector (Array Int SchemeValue)
                 | NativeFunction ([SchemeValue] -> ThrowsSchemeError SchemeValue)
                 | IONativeFunction (SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue)
                 | Function { params  :: [String]
                            , vararg  :: Maybe String
                            , body    :: [Command]
                            , closure :: SchemeEnvironment
                            }
                 | Port Handle

showVal :: SchemeValue -> String
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
showVal (IONativeFunction _) = "<native function>"
showVal (Function {params = args, vararg = varargs}) =
  "(lambda (" ++ unwords args ++
  (case varargs of
      Nothing  -> ""
      Just arg -> " . " ++ arg) ++ ") ... )"

showVal (Port _)             = "<port>"

showVal (List list)            = concat ["(", contents, ")"]
  where
    contents = unwords $ map showVal list
showVal (DottedList list last) = concat [ "("
                                        , contents
                                        , " . "
                                        , showVal last
                                        , ")"
                                        ]
  where
    contents = unwords $ map showVal list

instance Show SchemeValue where
  show = showVal

type SchemeError         = LangError SchemeValue
type ThrowsSchemeError   = ThrowsError SchemeValue
type IOThrowsSchemeError = IOThrowsError SchemeValue

type SchemeEnvironment   = Environment SchemeValue
