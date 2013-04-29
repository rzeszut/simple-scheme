module Scheme.Primitives (primitives) where

import Scheme.Data
import Scheme.Error
import Scheme.Primitives.Boolean
import Scheme.Primitives.Char
import Scheme.Primitives.List
import Scheme.Primitives.Number
import Scheme.Primitives.String
import Scheme.Primitives.Symbol
import Scheme.Primitives.Vector
import Scheme.Primitives.Eqv
import Scheme.Primitives.Equal

primitives :: [(String, [SchemeValue] -> ThrowsError SchemeValue)]
primitives =
  [
    -- generic equality predicates
    ("eqv?", eqv)
  , ("eq?",  eqv)
  , ("equal?", equal)

    -- numbers
  , ("number?",   unaryOp numberp)
  , ("complex?",  unaryOp complexp)
  , ("real?",     unaryOp realp)
  , ("rational?", unaryOp rationalp)
  , ("integer?",  unaryOp integerp)
  , ("+",         numericBinop (+))
  , ("-",         numericBinop (-))
  , ("*",         numericBinop (*))
  , ("/",         numericBinop div)
  , ("mod",       numericBinop mod)
  , ("quotient",  numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=",         numBoolBinop (==))
  , ("<",         numBoolBinop (<))
  , (">",         numBoolBinop (>))
  , ("<=",        numBoolBinop (<=))
  , (">=",        numBoolBinop (>=))

    -- booleans
  , ("boolean?", unaryOp booleanp)
  , ("not",      unaryOp not')
  , ("&&",       booleanBoolBinop (&&))
  , ("||",       booleanBoolBinop (||))

    -- lists
  , ("pair?",  unaryOp pairp)
  , ("cons",   cons)
  , ("car",    unaryThrowingOp car)
  , ("cdr",    unaryThrowingOp cdr)
  , ("null?",  unaryOp nullp)
  , ("list?",  unaryOp listp)
    
    -- symbols
  , ("symbol?",        unaryOp symbolp)
  , ("symbol->string", unaryThrowingOp symbol2string)
  , ("string->symbol", unaryThrowingOp string2symbol)
    
    -- characters
  , ("char?",   unaryOp charp)
  , ("char=?",  charBoolBinop (==))
  , ("char<?",  charBoolBinop (<))
  , ("char>?",  charBoolBinop (>))
  , ("char<=?", charBoolBinop (<=))
  , ("char>=?", charBoolBinop (>=))
    
    -- strings
  , ("string?",   unaryOp stringp)
  , ("string=?",  stringBoolBinop (==))
  , ("string<?",  stringBoolBinop (<))
  , ("string>?",  stringBoolBinop (>))
  , ("string<=?", stringBoolBinop (<=))
  , ("string>=?", stringBoolBinop (>=))
    
    -- vectors
  , ("vector?", unaryOp vectorp)
  ]

boolBinop :: (SchemeValue -> ThrowsError a) -> (a -> a -> Bool) -> [SchemeValue] -> ThrowsError SchemeValue
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise       = do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return . Boolean $ left `op` right

-- TODO: proper unpackers
-- these unpackers are used by boolBinop AND equal?
-- which is rather wrong, because equal? unpackers should be really lax
-- e.g. (equals? "2" 2.0) --> #t
-- but (string= "2" 2.0) should throw a type mismatch error
-- therefore, there should be two separate sets of unpackers

-- use Haskell Num class (?)
unpackNum :: SchemeValue -> ThrowsError Integer
unpackNum (Integer i) = return i
unpackNum notNum      = throwError $ TypeMismatch "number" notNum

unpackString :: SchemeValue -> ThrowsError String
unpackString (String s) = return s
unpackString notStr     = throwError $ TypeMismatch "string" notStr

unpackBoolean :: SchemeValue -> ThrowsError Bool
unpackBoolean (Boolean b) = return b
unpackBoolean notBool     = throwError $ TypeMismatch "boolean" notBool

unpackChar :: SchemeValue -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar  = throwError $ TypeMismatch "char" notChar

numBoolBinop     = boolBinop unpackNum
stringBoolBinop  = boolBinop unpackString
booleanBoolBinop = boolBinop unpackBoolean
charBoolBinop    = boolBinop unpackChar

numericBinop :: (Integer -> Integer -> Integer) -> [SchemeValue] -> ThrowsError SchemeValue
numericBinop _  singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params
                                >>= return . Integer . foldl1 op

unaryOp :: (SchemeValue -> SchemeValue) -> [SchemeValue] -> ThrowsError SchemeValue
unaryOp f [v]  = return $ f v
unaryOp _ vals = throwError $ NumArgs 1 vals

unaryThrowingOp :: (SchemeValue -> ThrowsError SchemeValue) -> [SchemeValue] -> ThrowsError SchemeValue
unaryThrowingOp f [v]  = f v
unaryThrowingOp _ vals = throwError $ NumArgs 1 vals
