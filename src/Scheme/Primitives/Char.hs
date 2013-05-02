module Scheme.Primitives.Char (charPrimitives) where

import Data.Char (toLower, toUpper, chr, ord)
import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

charPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
charPrimitives = [ ("char?",         unaryOp charp)
                 , ("char=?",        charBoolBinop (==))
                 , ("char<?",        charBoolBinop (<))
                 , ("char>?",        charBoolBinop (>))
                 , ("char<=?",       charBoolBinop (<=))
                 , ("char>=?",       charBoolBinop (>=))
                 , ("char->integer", unaryThrowingOp char2integer)
                 , ("integer->char", unaryThrowingOp integer2char)
                 , ("char-upcase",   unaryThrowingOp charUpcase)
                 , ("char-downcase", unaryThrowingOp charDowncase)
                 ]

-- TODO: errors

charp (Char _) = Boolean True
charp _        = Boolean False

char2integer (Char c) = return . Integer . toInteger $ ord c

integer2char (Integer i) = return . Char . chr $ fromInteger i

charUpcase (Char c) = return . Char $ toUpper c

charDowncase (Char c) = return . Char $ toLower c

charBoolBinop = boolBinop unpacker
  where
    unpacker (Char c) = return c
    unpacker notChar  = throwError $ TypeMismatch "char" notChar
