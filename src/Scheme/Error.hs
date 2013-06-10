module Scheme.Error (
  LangError(..)
  , ThrowsError(..)
  , IOThrowsError(..)
  , trapError
  , extractValue
  , liftThrows
  , runIOThrows
  , liftScanner
  , liftParser
  , throwError
  , catchError
  ) where

import Control.Monad.Error (Error(..), ErrorT(..), throwError, catchError, runErrorT)
import Scheme.Scanner (ScannerError)
import Scheme.Parser (ParserError)

-- | Language error datatype
data LangError a = Scanner ScannerError
                 | Parser ParserError        -- ^ Parser error
                 | NumArgs Integer [a]       -- ^ Invalid number of arguments
                 | TypeMismatch String a     -- ^ Invalid type
                 | NotFunction String a      -- ^ Value is not a function
                 | UnboundVar String String  -- ^ Unbound variable
                 | Default String            -- ^ Default error message

showError :: (Show a) => LangError a -> String
showError (Scanner error)               = show error
showError (Parser error)                = show error
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
showError (NotFunction message func)    = concat [message, ": ", show func]
showError (UnboundVar message varname)  = concat [message, ": ", varname]
showError (Default message)             = "Error: " ++ message

instance (Show a) => Show (LangError a) where
  show = showError

instance Error (LangError a) where
  noMsg  = Default "An error has occured."
  strMsg = Default

type ThrowsError a = Either (LangError a)

type IOThrowsError a = ErrorT (LangError a) IO

trapError action = catchError action (return . show)

extractValue :: ThrowsError t a -> a
extractValue (Right x) = x

liftThrows :: ThrowsError t a -> IOThrowsError t a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: (Show t) => IOThrowsError t String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

liftScanner :: Either ScannerError t -> IOThrowsError a t
liftScanner (Left err)  = throwError $ Scanner err
liftScanner (Right val) = return val

liftParser :: Either ParserError t -> IOThrowsError a t
liftParser (Left err)  = throwError $ Parser err
liftParser (Right val) = return val
