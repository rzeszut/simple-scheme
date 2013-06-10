{
module Scheme.Parser (
  -- * AST
  Ident
  , Program(..)
  , Command(..)
  , Definition (..)
  , Params(..)
  , Expression(..)
  , Special(..)
  , Literal(..)
  , Quotation(..)
  , SelfEvaluating(..)
  , FunctionCall(..)
  , Lambda(..)
  , LambdaParams(..)
  , Binding(..)
  , IfElse(..)
  , Body(..)
  , CondClause(..)
  , QuasiQuotation(..)
  , UnQuotation(..)
  , UnQuotationSplicing(..)
  , Template(..)
  , ListTemplate(..)
  , VectorTemplate(..)
  , TemplateOrSplice(..)
  , Datum(..)
  , SimpleDatum(..)
  , Number(..)
  , List(..)
  , Vector(..)
    -- * Parser Error handling
  , ParserError(..)
  , ThrowsParserError
    -- * Parsing
  , parse
  ) where

import Control.Monad.Error (Error(..), throwError)
import Data.Complex (Complex((:+)))

import qualified Scheme.Scanner as S
}

%monad { ThrowsParserError }
%name parse
%tokentype { S.Token S.SchemeToken }
%error { parseError }

%token
  "("  { S.Token _ S.LeftParen }
  ")"  { S.Token _ S.RightParen }
  "#(" { S.Token _ S.VectorBegin }
  "."  { S.Token _ S.Dot }
  
  "'"                { S.Token _ S.Quote }
  "quote"            { S.Token _ (S.Symbol "quote") }
  "`"                { S.Token _ S.QuasiQuote }
  "quasiquote"       { S.Token _ (S.Symbol "quasiquote") }
  ","                { S.Token _ S.UnQuote }
  "unquote"          { S.Token _ (S.Symbol "unquote")}
  ",@"               { S.Token _ S.UnQuoteSplicing }
  "unquote-splicing" { S.Token _ (S.Symbol "unquote-splicing") }

  "lambda" { S.Token _ (S.Symbol "lambda") }
  "set!"   { S.Token _ (S.Symbol "set!") }
  "if"     { S.Token _ (S.Symbol "if") }
  "begin"  { S.Token _ (S.Symbol "begin") }
  "or"     { S.Token _ (S.Symbol "or") }
  "and"    { S.Token _ (S.Symbol "and") }
  "cond"   { S.Token _ (S.Symbol "cond") }
  "else"   { S.Token _ (S.Symbol "else") }
  "define" { S.Token _ (S.Symbol "define") }
  "let"    { S.Token _ (S.Symbol "let") }
  "let*"   { S.Token _ (S.Symbol "let*") }
  "delay"  { S.Token _ (S.Symbol "delay") }

  integer   { S.Token _ (S.Integer $$) }
  rational  { S.Token _ (S.Rational $$) }
  float     { S.Token _ (S.Float $$) }
  complex   { S.Token _ (S.Complex $$) }
  boolean   { S.Token _ (S.Boolean $$) }
  character { S.Token _ (S.Character $$) }
  string    { S.Token _ (S.String $$) }

  symbol    { S.Token _ (S.Symbol $$) }
%%

list(p) :
  p list(p) { $1 : $2 }
  |         { [] }

list1(p) :
  p            { [$1] }
  | p list1(p) { $1 : $2 }

Program :
  list(Command) { Program $1 }

Command :
  Expression { Expression $1 }
  | Definition { Definition $1 }

Definition:
  "(" "define" symbol Expression ")"            { VarDefinition $3 $4 }
  | "(" "define" "(" symbol Params ")" Body ")" { FunDefinition $4 $5 $7 }

Params :
  list(symbol)               { Params $1 }
  | list(symbol) "." symbol { VarArgParams $1 $3 }

Expression :
  symbol         { Ident $1 }
  | Literal      { Literal $1 }
  | FunctionCall { FunctionCall $1 }
  | Lambda       { Lambda $1 }
  | Special      { Special $1 }

Special : 
  "(" "set!" symbol Expression ")"             { Set $3 $4 }
  | "(" "if" Expression Expression IfElse ")"  { If $3 $4 $5 }
  | "(" "begin" list1(Expression) ")"          { Begin $3 }
  | "(" "or" list(Expression) ")"              { Or $3 }
  | "(" "and" list(Expression) ")"             { And $3 }
  | "(" "cond" list1(CondClause) ")"           { Cond $3 }
  | "(" "let" "(" list1(Binding) ")" Body ")"  { Let $4 $6 }
  | "(" "let*" "(" list1(Binding) ")" Body ")" { LetRec $4 $6 }
  | "(" "delay" Expression ")"                 { Delay $3 }
  | QuasiQuotation                             { QuasiQuotation $1 }

Literal :
  Quotation        { Quotation $1 }
  | SelfEvaluating { SelfEvaluating $1 }

Quotation :
  "'" Datum               { Quote $2 }
  | "(" "quote" Datum ")" { Quote $3 }

SelfEvaluating :
  Number      { LNumber $1 }
  | boolean   { LBoolean $1 }
  | character { LCharacter $1 }
  | string    { LString $1 }
  | Vector    { LVector $1 }

FunctionCall :
  "(" Expression list(Expression) ")" { FuncCall $2 $3 }

Lambda :
  "(" "lambda" LambdaParams Body ")" { LambdaFun $3 $4 }
  
LambdaParams :
  symbol                             { LParamsList $1 }
  | "(" list(symbol) ")"             { LParams $2 }
  | "(" list1(symbol) "." symbol ")" { LVarArgParams $2 $4 }

IfElse :
  Expression { Else $1 }
  |          { Empty }

Binding :
  "(" symbol Expression ")" { Binding $2 $3 }

Body :
  list1(Command) { Body $1 }

CondClause :
  "(" Expression Expression ")" { CondClause $2 $3 }
  | "(" "else" Expression ")"   { CondElseClause $3 }

QuasiQuotation :
  "`" Template                    { QQuote $2 }
  | "(" "quasiquote" Template ")" { QQuote $3 }

UnQuotation :
  "," Expression                 { UnQuote $2 }
  | "(" "unquote" Expression ")" { UnQuote $3 }

UnQuotationSplicing :
  ",@" Expression                         { UnQuoteSplicing $2 }
  | "(" "unquote-splicing" Expression ")" { UnQuoteSplicing $3 }

Template :
  SimpleDatum      { SimpleDatum $1 }
  | ListTemplate   { ListTemplate $1 }
  | VectorTemplate { VectorTemplate $1 }
  | "'" Template   { QuotedTemplate $2 }
  | UnQuotation    { UnQuotation $1 }

ListTemplate :
  "(" list(TemplateOrSplice) ")"                 { ListTmp $2 }
  | "(" list1(TemplateOrSplice) "." Template ")" { DottedListTmp $2 $4 }

VectorTemplate :
  "#(" list(TemplateOrSplice) ")" { VectorTmp $2 }

TemplateOrSplice :
  Template              { Template $1 }
  | UnQuotationSplicing { Splice $1 }

Datum :
  SimpleDatum { Simple $1 }
  | List      { ListDatum $1 }
  | Vector    { VectorDatum $1 }

SimpleDatum :
  Number      { DNumber $1 }
  | boolean   { DBoolean $1 }
  | character { DCharacter $1 }
  | string    { DString $1 }
  | symbol    { DSymbol $1 }

Number :
  integer    { NInteger $1 }
  | rational { NRational $1 }
  | float    { NFloat $1 }
  | complex  { NComplex $1 }

List :
  "(" list(Datum) ")"              { DList $2 }
  | "(" list1(Datum) "." Datum ")" { DDottedList $2 $4 }

Vector :
  "#(" list(Datum) ")" { DVector $2 }

{
  
-- AST
type Ident = String

data Program = Program [Command]
               deriving (Show)

data Command = Expression Expression
             | Definition Definition
             deriving (Show)

data Definition = VarDefinition Ident Expression
                | FunDefinition Ident Params Body
                deriving (Show)

data Params = Params [Ident]
            | VarArgParams [Ident] Ident
            deriving (Show)

data Expression = Ident Ident
                | Literal Literal
                | FunctionCall FunctionCall
                | Lambda Lambda
                | Special Special
                deriving (Show)

data Literal = Quotation Quotation
             | SelfEvaluating SelfEvaluating
             deriving (Show)

data Quotation = Quote Datum
               deriving (Show)

data SelfEvaluating = LNumber Number
                    | LBoolean Bool
                    | LCharacter Char
                    | LString String
                    | LVector Vector
                    deriving (Show)

data FunctionCall = FuncCall Expression [Expression]
                  deriving (Show)

data Lambda = LambdaFun LambdaParams Body
              deriving (Show)

data LambdaParams = LParamsList Ident
                  | LParams [Ident]
                  | LVarArgParams [Ident] Ident
                  deriving (Show)

data Body = Body [Command]
            deriving (Show)

data Special = Set Ident Expression
             | If Expression Expression IfElse
             | Begin [Expression]
             | Or [Expression]
             | And [Expression]
             | Cond [CondClause]
             | Let [Binding] Body
             | LetRec [Binding] Body
             | Delay Expression
             | QuasiQuotation QuasiQuotation
             deriving (Show)

data Binding = Binding { getName :: Ident
                       , getExpr :: Expression
                       }
             deriving (Show)

data IfElse = Else Expression
            | Empty
            deriving (Show)

data CondClause = CondClause Expression Expression
                | CondElseClause Expression
                deriving (Show)

data QuasiQuotation = QQuote Template
                    deriving (Show)

data UnQuotation = UnQuote Expression
                 deriving (Show)

data UnQuotationSplicing = UnQuoteSplicing Expression
                         deriving (Show)

data Template = SimpleDatum SimpleDatum
              | ListTemplate ListTemplate
              | VectorTemplate VectorTemplate
              | QuotedTemplate Template
              | UnQuotation UnQuotation
              deriving (Show)

data ListTemplate = ListTmp [TemplateOrSplice]
                  | DottedListTmp [TemplateOrSplice] Template
                  deriving (Show)

data VectorTemplate = VectorTmp [TemplateOrSplice]
                    deriving (Show)

data TemplateOrSplice = Template Template
                      | Splice UnQuotationSplicing
                      deriving (Show)

data Datum = Simple SimpleDatum
           | ListDatum List
           | VectorDatum Vector
           deriving (Show)

data SimpleDatum = DNumber Number
                 | DBoolean Bool
                 | DCharacter Char
                 | DString String
                 | DSymbol Ident
                 deriving (Show)

data Number = NInteger Integer
            | NRational Rational
            | NFloat Double
            | NComplex (Complex Double)
            deriving (Show)

data List = DList [Datum]
          | DDottedList [Datum] Datum
          deriving (Show)

data Vector = DVector [Datum]
            deriving (Show)

-- errors
data ParserError = ParserError (S.Token S.SchemeToken)
                 | Default String

showError :: ParserError -> String
showError (ParserError token) = concat [ "parse error at "
                                       , show $ S.getLineNum token
                                       , " line, "
                                       , show $ S.getColumnNum token
                                       , " column: "
                                       , show $ S.getToken token
                                       ]
showError (Default str) = str

instance Show ParserError where
  show = showError

instance Error ParserError where
  noMsg  = Default "A parser error has occured."
  strMsg = Default

type ThrowsParserError = Either ParserError

parseError :: [S.Token S.SchemeToken] -> ThrowsParserError a
parseError tokens = throwError $ ParserError $ head tokens

}
