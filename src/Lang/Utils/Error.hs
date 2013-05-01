module Lang.Utils.Error (
  LangError(..)
  , ThrowsError(..)
  , IOThrowsError(..)
  , trapError
  , extractValue
  , liftThrows
  , runIOThrows
  , throwError
  , catchError
  ) where

import Control.Monad.Error (Error(..), ErrorT(..), throwError, catchError, runErrorT)
import Text.ParserCombinators.Parsec (ParseError)

-- | Language error datatype
data LangError a = NumArgs Integer [a]       -- ^ Invalid number of arguments
                 | TypeMismatch String a     -- ^ Invalid type
                 | Parser ParseError         -- ^ Parser error
                 | BadSpecialForm String a   -- ^ Invalid form
                 | NotFunction String String -- ^ Symbol is not a function
                 | UnboundVar String String  -- ^ Unbound variable
                 | Default String            -- ^ Default error message

showError :: (Show a) => LangError a -> String
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

