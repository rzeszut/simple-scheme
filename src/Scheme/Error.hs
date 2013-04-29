module Scheme.Error ( SchemeError(..)
                    , ThrowsError(..)
                    , trapError
                    , extractValue
                    , throwError
                    , catchError
                    ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec
import Scheme.Data

data SchemeError = NumArgs Integer [SchemeValue]
                 | TypeMismatch String SchemeValue
                 | Parser ParseError
                 | BadSpecialForm String SchemeValue
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

showError :: SchemeError -> String
showError (NumArgs expected found)      = concat [ "Expected "
                                                 , show expected
                                                 , " args; found values "
                                                 , unwords . map show $ found
                                                 ]
showError (TypeMismatch expected found) = concat [ "Invalid type; expected "
                                                 , expected
                                                 , ", found "
                                                 , show found
                                                 ]
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = concat [message, ": ", show form]
showError (NotFunction message func)    = concat [message, ": ", show func]
showError (UnboundVar message varname)  = concat [message, ": ", varname]
showError (Default message)             = message

instance Show SchemeError where
  show = showError

instance Error SchemeError where
  noMsg  = Default "An error has occured."
  strMsg = Default

type ThrowsError = Either SchemeError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x
