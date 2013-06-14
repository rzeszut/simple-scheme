module Scheme.Primitives.String (stringPrimitives) where

import Data.Char (toLower)
import Scheme.Error
import Scheme.Data
import Scheme.Primitives.Common

stringPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
stringPrimitives = [ ("string?",       unaryFunction stringp)
                   , ("string-length", stringLength)
                   , ("string-ref",    stringRef)
                     
                   , ("string=?",     stringBoolBinop (==))
                   , ("string<?",     stringBoolBinop (<))
                   , ("string>?",     stringBoolBinop (>))
                   , ("string<=?",    stringBoolBinop (<=))
                   , ("string>=?",    stringBoolBinop (>=))
                   , ("string-ci=?",  stringCiBoolBinop (==))
                   , ("string-ci<?",  stringCiBoolBinop (<))
                   , ("string-ci>?",  stringCiBoolBinop (>))
                   , ("string-ci<=?", stringCiBoolBinop (<=))
                   , ("string-ci>=?", stringCiBoolBinop (>=))

                   , ("substring",     substringProc)
                   , ("string-append", stringAppend)
                   , ("string->list",  string2list)
                   , ("list->string",  list2string)
                   , ("string-copy",   stringCopy)
                   ]

stringp (String _) = Boolean True
stringp _          = Boolean False

substringProc :: [SchemeValue] -> ThrowsSchemeError SchemeValue
substringProc [(String str), (Integer start), (Integer end)]
  | start >= 0 && end >= start && (toInteger $ length str) > end =
    return . String $ drop (fromInteger start) $ take (fromInteger end) str
  | otherwise = throwError $ Default "Invalid 'start' or 'end' parameter."
substringProc [notStr, (Integer _), (Integer _)] = throwError $ TypeMismatch "string" notStr
substringProc [_, notInt, (Integer _)]           = throwError $ TypeMismatch "integer" notInt
substringProc [_, _, notInt]                     = throwError $ TypeMismatch "integer" notInt
substringProc args                               = throwError $ NumArgs 3 args

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

string2list = stringUnary (map Char) (return . List)
list2string = makeUnaryFunction unpackList
              (map unpackChar)
              (\l -> sequence l >>= return . String)

stringCopy = stringUnary (id) (return . String)

-- helpers
stringBoolBinop       = makeBinaryBoolFunction unpackString
stringCiBoolBinop fun = stringBoolBinop (\s1 s2 -> (map toLower s1) `fun` (map toLower s2))
stringUnary           = makeUnaryFunction unpackString
