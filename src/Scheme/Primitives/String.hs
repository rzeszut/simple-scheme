module Scheme.Primitives.String (stringPrimitives) where

import Lang.Utils.Error
import Scheme.Data
import Scheme.Primitives.Common

stringPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
stringPrimitives = [ ("string?",       unaryFunction stringp)
                   , ("string-length", stringLength)
                   , ("string-ref",    stringRef)
                   , ("string=?",      stringBoolBinop (==))
                   , ("string<?",      stringBoolBinop (<))
                   , ("string>?",      stringBoolBinop (>))
                   , ("string<=?",     stringBoolBinop (<=))
                   , ("string>=?",     stringBoolBinop (>=))
                   , ("string-append", stringAppend)
                   , ("string->list",  string2list)
                   , ("list->string",  list2string)
                   ]

stringp (String _) = Boolean True
stringp _          = Boolean False

stringLength = stringUnary length (return . Integer . toInteger)

stringRef [(String s), (Integer k)]
  | (fromInteger k) >= length s = throwError $ Default "invalid index"
stringRef args = makeBinaryFunction unpackString unpackInteger
                 (\s k -> s !! (fromInteger k))
                 (return . Char) args

stringAppend :: [SchemeValue] -> ThrowsSchemeError SchemeValue
stringAppend s = do
  strings <- sequence $ map unpackString s
  return . String $ concat strings

string2list = stringUnary (map Char) (return . toLispList)
list2string = makeUnaryFunction (return . fromLispList)
              (map unpackChar)
              (\l -> sequence l >>= return . String)

stringBoolBinop = makeBinaryBoolFunction unpackString
stringUnary     = makeUnaryFunction unpackString
