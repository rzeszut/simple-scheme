module Scheme.Primitives.String (stringPrimitives) where

import Data.Char (toLower)
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
                   , ("string-ci=?",   stringCiBoolBinop (==))
                   , ("string-ci<?",   stringCiBoolBinop (<))
                   , ("string-ci>?",   stringCiBoolBinop (>))
                   , ("string-ci<=?",  stringCiBoolBinop (<=))
                   , ("string-ci>=?",  stringCiBoolBinop (>=))
                   , ("string-append", stringAppend)
                   , ("string->list",  string2list)
                   , ("list->string",  list2string)
                   ]

stringp (String _) = Boolean True
stringp _          = Boolean False

-- make-string

stringLength = stringUnary length (return . Integer . toInteger)

stringRef [(String s), (Integer k)]
  | (fromInteger k) >= length s = throwError $ Default "invalid index"
stringRef args = makeBinaryFunction unpackString unpackInteger
                 (\s k -> s !! (fromInteger k))
                 (return . Char) args

-- string-set!

stringAppend :: [SchemeValue] -> ThrowsSchemeError SchemeValue
stringAppend s = do
  strings <- sequence $ map unpackString s
  return . String $ concat strings

string2list = stringUnary (map Char) (return . toLispList)
list2string = makeUnaryFunction (return . fromLispList)
              (map unpackChar)
              (\l -> sequence l >>= return . String)

-- string-fill!

stringBoolBinop       = makeBinaryBoolFunction unpackString
stringCiBoolBinop fun = stringBoolBinop (\s1 s2 -> (map toLower s1) `fun` (map toLower s2))
stringUnary           = makeUnaryFunction unpackString
