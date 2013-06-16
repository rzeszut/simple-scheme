module Scheme.Eval ( eval
                   , apply
                   ) where

import Control.Monad (liftM, mapM)
import Control.Monad.Trans (liftIO)
import Data.Array (listArray)
import Scheme.Data
import Scheme.Error
import Scheme.Environment (getVar, setVar, defineVar, bindVars, newEnv)
import Scheme.Parser hiding (ParserError(..), ThrowsParserError, parse)

eval :: SchemeEnvironment -> Program -> IOThrowsSchemeError SchemeValue
eval env (Program commands) = liftM last $ mapM (evalCommand env) commands

evalCommand :: SchemeEnvironment -> Command -> IOThrowsSchemeError SchemeValue
evalCommand env (Expression exp) = evalExpression env exp
evalCommand env (Definition def) = evalDefinition env def

evalDefinition :: SchemeEnvironment -> Definition -> IOThrowsSchemeError SchemeValue
evalDefinition env (VarDefinition name exp) =
  evalExpression env exp >>= defineVar env name
evalDefinition env (FunDefinition name paramsList body) =
  makeFunction params vararg body env >>= defineVar env name
  where
    (params, vararg) = case paramsList of
      Params p         -> (p, Nothing)
      VarArgParams p v -> (p, Just v)

evalExpression :: SchemeEnvironment -> Expression -> IOThrowsSchemeError SchemeValue
evalExpression env (Ident name)     = getVar env name
evalExpression env (Literal l)      = evalLiteral l
evalExpression env (FunctionCall c) = evalCall env c
evalExpression env (Lambda l)       = evalLambda env l
evalExpression env (Special s)      = evalSpecial env s

evalLiteral :: Literal -> IOThrowsSchemeError SchemeValue
evalLiteral (SelfEvaluating se) = evalSelfEvaluating se
  where
    evalSelfEvaluating (LNumber num)  = return $ evalNumber num
    evalSelfEvaluating (LBoolean b)   = return $ Boolean b
    evalSelfEvaluating (LCharacter c) = return $ Char c
    evalSelfEvaluating (LString s)    = return $ String s
    evalSelfEvaluating (LVector v)    = evalVector v
evalLiteral (Quotation (Quote datum)) = evalDatum datum

evalDatum :: Datum -> IOThrowsSchemeError SchemeValue
evalDatum (Simple datum) = evalSimpleDatum datum
  where
    evalSimpleDatum (DNumber num)  = return $ evalNumber num
    evalSimpleDatum (DBoolean b)   = return $ Boolean b
    evalSimpleDatum (DCharacter c) = return $ Char c
    evalSimpleDatum (DString s)    = return $ String s
    evalSimpleDatum (DSymbol s)    = return $ Symbol s
evalDatum (ListDatum list) = evalList list
  where
    evalList (DList list)            = (sequence $ map evalDatum list) >>= (return . List)
    evalList (DDottedList list last) = do
      car <- sequence $ map evalDatum list
      cdr <- evalDatum last
      return $ DottedList car cdr
evalDatum (VectorDatum vec) = evalVector vec

evalVector :: Vector -> IOThrowsSchemeError SchemeValue
evalVector (DVector values) = (sequence $ map evalDatum values) >>= makeVector

evalNumber :: Number -> SchemeValue
evalNumber (NInteger int) = Integer int
evalNumber (NRational r)  = Rational r
evalNumber (NFloat f)     = Float f
evalNumber (NComplex c)   = Complex c

evalCall :: SchemeEnvironment -> FunctionCall -> IOThrowsSchemeError SchemeValue
evalCall env (FuncCall fun args) = do
  function <- evalExpression env fun
  argVals  <- mapM (evalExpression env) args
  apply env function argVals

evalLambda :: SchemeEnvironment -> Lambda -> IOThrowsSchemeError SchemeValue
evalLambda env (LambdaFun paramsList body) = 
  makeFunction params vararg body env
  where
    (params, vararg) = case paramsList of
      LParamsList v     -> ([], Just v)
      LParams p         -> (p, Nothing)
      LVarArgParams p v -> (p, Just v)

evalSpecial :: SchemeEnvironment -> Special -> IOThrowsSchemeError SchemeValue
evalSpecial env (Set name val) =
  evalExpression env val >>= setVar env name

evalSpecial env (If cond true false) =
  do result <- evalExpression env cond
     if isTrue result then evalExpression env true
       else evalElse false
  where
    evalElse (Else exp) = evalExpression env exp
    evalElse (Empty)    = return $ Boolean False

evalSpecial env (Begin exps) =
  liftM last $ mapM (evalExpression env) exps

evalSpecial env (Or exps) = evalOr exps
  where
    evalOr []       = return $ Boolean False
    evalOr (e:rest) = do val <- evalExpression env e
                         if isTrue val then return val
                           else evalOr rest

evalSpecial env (And exps) = evalAnd (Boolean True) exps
  where
    evalAnd last []       = return last
    evalAnd last (e:rest) = do val <- evalExpression env e
                               if isTrue val then evalAnd val rest
                                 else return val

evalSpecial env (Cond clauses)      = evalCond env clauses

evalSpecial env (Let bindings (Body body)) = do
  vals   <- mapM (evalExpression env) exprs
  newEnv <- liftIO $ bindVars env (zip idents vals)
  liftM last $ mapM (evalCommand newEnv) body
  where
    idents = map getName bindings
    exprs  = map getExpr bindings

evalSpecial env (LetRec bindings (Body body)) = do
  newEnv <- liftIO $ newEnv env
  mapM (newVar newEnv) (zip idents exprs)
  liftM last $ mapM (evalCommand newEnv) body
  where
    newVar env (var, expr) = do
      val <- evalExpression env expr
      defineVar env var val
    idents = map getName bindings
    exprs  = map getExpr bindings

evalSpecial env (Delay expr) = return $ Function [] Nothing [Expression expr] env

evalSpecial env (QuasiQuotation qq) = evalQQ env qq

evalCond :: SchemeEnvironment -> [CondClause] -> IOThrowsSchemeError SchemeValue
evalCond env [] = throwError $ Default "Ill-formed cond expression."
evalCond env (c:rest) = case c of
  CondElseClause exp  -> evalExpression env exp
  CondClause cond exp -> do val <- evalExpression env cond
                            if isTrue val then evalExpression env exp
                              else evalCond env rest

evalQQ :: SchemeEnvironment -> QuasiQuotation -> IOThrowsSchemeError SchemeValue
evalQQ env (QQuote template) = evalTemplate env template

evalTemplate :: SchemeEnvironment -> Template -> IOThrowsSchemeError SchemeValue
evalTemplate env (SimpleDatum datum)   = evalDatum (Simple datum)
evalTemplate env (ListTemplate list)   = evalListTemplate env list
evalTemplate env (VectorTemplate vec)  = evalVectorTemplate env vec
evalTemplate env (QuotedTemplate temp) = do
  template <- evalTemplate env temp
  return $ List [Symbol "quote", template]
evalTemplate env (UnQuotation (UnQuote e)) = evalExpression env e

evalListTemplate :: SchemeEnvironment -> ListTemplate -> IOThrowsSchemeError SchemeValue
evalListTemplate env (ListTmp ts) = do
  evaled <- mapM (evalTemplateOrSplice env) ts
  return . List $ concat evaled
evalListTemplate env (DottedListTmp ts last) = do
  evaled     <- mapM (evalTemplateOrSplice env) ts
  lastEvaled <- evalTemplate env last
  return $ DottedList (concat evaled) lastEvaled

evalVectorTemplate :: SchemeEnvironment -> VectorTemplate -> IOThrowsSchemeError SchemeValue
evalVectorTemplate env (VectorTmp ts) = do
  evaled <- mapM (evalTemplateOrSplice env) ts
  let lst = concat evaled
  makeVector lst

evalTemplateOrSplice :: SchemeEnvironment -> TemplateOrSplice -> IOThrowsSchemeError [SchemeValue]
evalTemplateOrSplice env (Template t) = do
  evaled <- evalTemplate env t
  return [evaled]
evalTemplateOrSplice env (Splice (UnQuoteSplicing expr)) = do
  list <- evalExpression env expr
  case list of
    List l -> return l
    _      -> throwError $ TypeMismatch "list" list

apply :: SchemeEnvironment -> SchemeValue -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
apply _   (NativeFunction fun) args   = liftThrows $ fun args
apply env (IONativeFunction fun) args = fun env args
apply _   (Function params vararg body closure) args =
  if num params /= num args && vararg == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args)
       >>= bindVarArg vararg
       >>= evalBody
  where
    remainingArgs      = drop (length params) args
    num                = toInteger . length
    evalBody env       = liftM last $ mapM (evalCommand env) body
    bindVarArg arg env = case arg of
      Nothing      -> return env
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
apply _ fun _ = throwError $ NotFunction "value is not a function" fun

makeFunction params varargs (Body body) env = return $ Function params varargs body env

isTrue :: SchemeValue -> Bool
isTrue (Boolean False) = False
isTrue _               = True
