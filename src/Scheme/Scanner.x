{
module Scheme.Scanner ( Token(..)
                      , getLineNum
                      , getColumnNum
                      , SchemeToken(..)
                      , ScannerError(..)
                      , ThrowsScannerError(..)
                      , scan
                      ) where

import Control.Monad.Error (Error(..), throwError)
import Data.Char (digitToInt)
import Data.Complex (Complex((:+)))
import Data.Ratio (Rational, (%), numerator, denominator)
import Data.List.Split (split, dropInitBlank, keepDelimsL, oneOf)
import Numeric (readOct, readHex, readInt)

-- TODO: char: newline, space
-- TODO: string: escape ""
}

%wrapper "posn"

$binDigit = [01]
$octDigit = [0-7]
$decDigit = [0-9]
$hexDigit = [0-9A-Fa-f]
$alpha    = [a-zA-Z]
$symbol   = [\!\$\%\&\|\*\+\-\/\:\<\=\>\?\@\^\_\~]
$stringChar = $printable # \"

@integer    = \-? $decDigit+
@binInteger = "#b" $binDigit+
@octInteger = "#o" $octDigit+
@decInteger = "#d" $decDigit+
@hexInteger = "#x" $hexDigit+

@rational   = \-? $decDigit+ \/ $decDigit+
@float      = \-? $decDigit+ \. $decDigit+ ("e" @integer)?
@complexNum = $decDigit+ (\. $decDigit+ ("e" @integer)?)?
@complex    = \-? @complexNum [\+\-] @complexNum "i"

@character = "#\" $printable $alpha*
@string    = \" $stringChar* \"
@symbol    = [$alpha $symbol] [$alpha $symbol $decDigit]*

tokens :-
  
  $white+ ;
  ";".* ;
  
  "("  { \p s -> Token p LeftParen }
  ")"  { \p s -> Token p RightParen }
  "#(" { \p s -> Token p VectorBegin }
  "."  { \p s -> Token p Dot }
  "'"  { \p s -> Token p Quote }
  "`"  { \p s -> Token p QuasiQuote }
  ","  { \p s -> Token p UnQuote }
  ",@" { \p s -> Token p UnQuoteSplicing }
  
  @integer    { \p s -> Token p $ Integer $ readInteger s }
  @binInteger { \p s -> Token p $ Integer $ readBinInteger s }
  @octInteger { \p s -> Token p $ Integer $ readOctInteger s }
  @decInteger { \p s -> Token p $ Integer $ readDecInteger s }
  @hexInteger { \p s -> Token p $ Integer $ readHexInteger s }

  @rational { \p s -> Token p $ Rational $ readRational s }
  @float    { \p s -> Token p $ Float    $ readFloat s }
  @complex  { \p s -> Token p $ Complex  $ readComplex s}
  
  "#f" { \p s -> Token p $ Boolean False }
  "#t" { \p s -> Token p $ Boolean True }

  @character { \p s -> Token p $ Character $ readCharacter s }
  @string    { \p s -> Token p $ String    $ readString s }
  @symbol    { \p s -> Token p $ Symbol s }

{

data Token a = Token { getPosition :: AlexPosn
                     , getToken    :: a
                     }

getLineNum :: Token a -> Int
getLineNum (Token (AlexPn _ line _) _) = line

getColumnNum :: Token a -> Int
getColumnNum (Token (AlexPn _ _ col) _) = col

showToken :: (Show a) => Token a -> String
showToken (Token (AlexPn _ line col) token) =
  concat [ "(",  show line
         , ", ", show col
         , ") ", show token
         ]

instance (Show a) => Show (Token a) where
  show = showToken

data SchemeToken = LeftParen
                 | RightParen
                 | VectorBegin
                 | Dot
                 | Quote
                 | QuasiQuote
                 | UnQuote
                 | UnQuoteSplicing
                 | Integer Integer
                 | Rational Rational
                 | Float Double
                 | Complex (Complex Double)
                 | Boolean Bool
                 | Character Char
                 | String String
                 | Symbol String
                 deriving (Eq, Show)

readCharacter :: String -> Char
readCharacter str = toCharacter $ drop 2 str
  where
    toCharacter "newline" = '\n'
    toCharacter "space"   = ' '
    toCharacter s         = head s

readString :: String -> String
readString = escapeString . init . tail
  where
    escapeString []                = []
    escapeString ('\\' : c : rest) = case escapeChar c of
      Just esc -> esc : (escapeString rest)
      Nothing  -> escapeString rest
    escapeString (c : rest)        = c : (escapeString rest)

    escapeChar char = lookup char escaped
    escaped         = zip codes replacements
    codes           = ['"',  'n',  'r',  't',  'b',  'f',  '\\']
    replacements    = ['\"', '\n', '\r', '\t', '\b', '\f', '\\']


readInteger :: String -> Integer
readInteger = read

readBinInteger :: String -> Integer
readBinInteger str = readNum (readInt 2 isBin digitToInt) $ drop 2 str
  where
    isBin c  = c == '0' || c == '1'

readOctInteger :: String -> Integer
readOctInteger str = readNum readOct $ drop 2 str

readDecInteger :: String -> Integer
readDecInteger str = read $ drop 2 str

readHexInteger :: String -> Integer
readHexInteger str = readNum readHex $ drop 2 str

readNum reader = fst . head . reader

readRational :: String -> Rational
readRational str = num % den
  where
    (nstr, dstr) = break (== '/') str
    num          = readInteger nstr
    den          = readInteger $ tail dstr

readFloat :: String -> Double
readFloat = read

readComplex :: String -> Complex Double
readComplex str = re :+ im
  where
    [restr, imstr]  = split (dropInitBlank . keepDelimsL $ oneOf "+-") str
    toFloat ('+':s) = readFloat s
    toFloat s       = readFloat s
    re              = toFloat restr
    im              = toFloat $ init imstr

-- test main function
--main :: IO ()
--main = do
--  contents <- getContents
--  putStrLn . unlines . map show $ alexScanTokens contents

data ScannerError = ScannerError Int Int String
                  | Default String

showError :: ScannerError -> String
showError (ScannerError line col str) = concat [ "lexical error at "
                                               , (show line)
                                               , " line, "
                                               , (show col)
                                               , " column: "
                                               , str
                                               ]
showError (Default str) = str

instance Show ScannerError where
  show = showError

instance Error ScannerError where
  noMsg  = Default "An error has occured."
  strMsg = Default

type ThrowsScannerError = Either ScannerError

-- TODO: Either monad (error)
-- chain conses using Applicative (if it's possible) (liftA or <$>), or:
-- errList >>= (\list -> return (1:list))
scan :: String -> ThrowsScannerError [Token SchemeToken]
scan str = go (alexStartPos, '\n', [], str)
  where go inp@(pos, _, _, str) =
          case alexScan inp 0 of
            AlexEOF -> return []
            AlexError ((AlexPn _ line col), _, _, str) -> throwError $ ScannerError line col str
            AlexSkip  inp' len     -> go inp'
            AlexToken inp' len act -> (go inp') >>= \list ->
              return $ act pos (take len str) : list


}
