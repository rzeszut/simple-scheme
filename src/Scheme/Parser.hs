module Scheme.Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Error
import Numeric
import Data.Array
import Data.Char (digitToInt)
import Data.Ratio
import Data.Complex
import Scheme.Data
import Scheme.Error

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseString :: Parser SchemeValue
parseString = do
  char '"'
  x <- many $ chars
  char '"'
  return $ String x
  where
    chars          = escaped <|> noneOf "\""
    escaped        = char '\\' >> choice (zipWith escapeChar codes replacements)
    escapeChar c r = char c >> return r
    codes          = ['"',  'n',  'r',  't',  'b',  'f',  '\\']
    replacements   = ['\"', '\n', '\r', '\t', '\b', '\f', '\\']

parseSymbol :: Parser SchemeValue
parseSymbol = do
  first <- letter <|> symbol
  rest  <- many $ letter <|> digit <|> symbol
  return $ Symbol (first:rest)

parseBoolean :: Parser SchemeValue
parseBoolean = do string "#"
                  x <- oneOf "tf"
                  return $ case x of 
                    't' -> Boolean True
                    'f' -> Boolean False

parseInteger :: Parser SchemeValue
parseInteger = choice [parseDec, parseDec', parseOct, parseHex, parseBin]

-- TODO: negative numbers
parseDec  = parseInt digit readDec >>= toLispInteger
parseDec' = parseRadix "#d" >> parseDec
parseOct  = parseRadix "#o" >> parseInt octDigit readOct >>= toLispInteger
parseHex  = parseRadix "#x" >> parseInt hexDigit readHex >>= toLispInteger
parseBin  = parseRadix "#b" >> parseInt binDigit readBin >>= toLispInteger
    
parseRadix = try . string
parseInt parser reader = do
  n <- many1 parser
  return . readNum reader $ n
toLispInteger n = return $ Integer n

readNum reader = fst . head . reader

readBin  = readInt 2 isBin digitToInt
binDigit = satisfy isBin
isBin c  = c == '0' || c == '1'

parseFloat :: Parser SchemeValue
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return . Float . fst . head . readFloat $ (x ++ "." ++ y)

parseRational :: Parser SchemeValue
parseRational = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return . Rational $ (read x) % (read y)

toDouble (Float x)  = x
toDouble (Integer x) = fromIntegral x

-- TODO: negative numbers
parseComplex :: Parser SchemeValue
parseComplex = do
  re <- (try parseFloat <|> parseDec)
  char '+'
  im <- (try parseFloat <|> parseDec)
  char 'i'
  return . Complex $ (toDouble re) :+ (toDouble im)

parseChar :: Parser SchemeValue
parseChar = do
  string "#\\"
  char <- character
  return $ Char char
  where
    character = (try $ string "space" >> return ' ')
                <|> (try $ string "newline" >> return '\n')
                <|> anyChar

parseQuoted :: Parser SchemeValue
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ toLispList [Symbol "quote", x]

parseQuasiQuoted :: Parser SchemeValue
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ toLispList [Symbol "quasiquote", x]

parseUnQuote :: Parser SchemeValue
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ toLispList [Symbol "unquote", x]

toLispList :: [SchemeValue] -> SchemeValue
toLispList []     = Nil
toLispList (x:xs) = Cons x (toLispList xs)

parseList :: Parser SchemeValue
parseList = do
  char '(' >> spaces
  head <- sepEndBy parseExpr spaces1
  tail <- (char '.' >> spaces1 >> parseExpr) <|> return Nil
  spaces >> char ')'
  return $ makeLispList head tail
    where
      makeLispList [] _        = Nil
      makeLispList [car] cdr   = Cons car cdr
      makeLispList (x:xs) last = Cons x (makeLispList xs last)

parseVector :: Parser SchemeValue
parseVector = do
  string "#("
  values <- sepBy parseExpr spaces1
  char ')'
  return . Vector $ listArray (0, length values - 1) values

parseExpr :: Parser SchemeValue
parseExpr = parseSymbol
            <|> parseString
            <|> try parseComplex
            <|> try parseRational
            <|> try parseFloat
            <|> try parseInteger
            <|> try parseBoolean
            <|> try parseChar
            <|> try parseVector
            <|> parseQuoted
            <|> parseQuasiQuoted
            <|> parseUnQuote
            <|> parseList
readExpr :: String -> ThrowsError SchemeValue
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ Parser err
  Right val -> return val

