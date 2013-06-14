module Scheme.Primitives.IO ( ioPrimitives
                            , load
                            ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Scheme.Data
import Scheme.Error
import Scheme.Primitives.Common (ignoreEnvironment, unaryIoThrowingFunction)
import Scheme.Scanner (scan, defaultPos, Token(Token), SchemeToken(Quote))
import Scheme.Parser (parse)
import Scheme.Eval (eval)
import System.IO

ioPrimitives :: [(String, SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue)]
ioPrimitives = [ ("open-input-file",     ignoreEnvironment $ makePort ReadMode)
               , ("open-output-file",    ignoreEnvironment $ makePort WriteMode)
               , ("close-input-port",    ignoreEnvironment $ closePort)
               , ("close-output-port",   ignoreEnvironment $ closePort)
               , ("input-port?",         ignoreEnvironment $ unaryIoThrowingFunction inputPortp)
               , ("output-port?",        ignoreEnvironment $ unaryIoThrowingFunction outputPortp)
               , ("current-input-port",  ignoreEnvironment $ currentInputPort)
               , ("current-output-port", ignoreEnvironment $ currentOutputPort)

               , ("read",        readProc)
               , ("read-char",   ignoreEnvironment $ readChar)
               , ("peek-char",   ignoreEnvironment $ peekChar)
               , ("eof-object?", ignoreEnvironment $ unaryIoThrowingFunction eofObjectp)
               , ("char-ready?", ignoreEnvironment $ charReadyp)

               , ("write-char", ignoreEnvironment $ writeChar)
               , ("display",    ignoreEnvironment $ writeProc)
               , ("write",      ignoreEnvironment $ writeProc)

               , ("load", loadProc)
               ]

makePort :: IOMode -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [x]                  = throwError $ TypeMismatch "string" x
makePort _ xs                   = throwError $ NumArgs 1 xs

closePort :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
closePort [Port port] = liftIO $ hClose port >> (return $ Boolean True)
closePort [x]         = throwError $ TypeMismatch "port" x
closePort xs          = throwError $ NumArgs 1 xs

inputPortp :: SchemeValue -> IOThrowsSchemeError SchemeValue
inputPortp (Port port) = do
  readable <- liftIO $ hIsReadable port
  return $ Boolean readable
inputPortp _ = return $ Boolean False

outputPortp :: SchemeValue -> IOThrowsSchemeError SchemeValue
outputPortp (Port port) = do
  writable <- liftIO $ hIsWritable port
  return $ Boolean writable
outputPortp _ = return $ Boolean False

currentInputPort :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
currentInputPort []   = return $ Port stdin
currentInputPort args = throwError $ NumArgs 0 args

currentOutputPort :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
currentOutputPort []   = return $ Port stdout
currentOutputPort args = throwError $ NumArgs 0 args

readProc :: SchemeEnvironment -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
readProc env []          = readProc env [Port stdin]
readProc env [Port port] = do
  str    <- liftIO $ hGetLine port
  tokens <- liftIOScanner $ scan str
  d      <- liftParser    $ parse $ Token defaultPos Quote : tokens
  eval env d
readProc _ [notPort]     = throwError $ TypeMismatch "port" notPort
readProc _ args          = throwError $ NumArgs 1 args

readChar :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
readChar []          = readChar [Port stdin]
readChar [Port port] = do
  char <- liftIO $ hGetChar port
  return $ Char char
readChar [notPort]   = throwError $ TypeMismatch "port" notPort
readChar args        = throwError $ NumArgs 1 args

peekChar :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
peekChar []          = peekChar [Port stdin]
peekChar [Port port] = do
  char <- liftIO $ hLookAhead port
  return $ Char char
peekChar [notPort]   = throwError $ TypeMismatch "port" notPort
peekChar args        = throwError $ NumArgs 1 args

eofObjectp :: SchemeValue -> IOThrowsSchemeError SchemeValue
eofObjectp (Port p) = do
  eof <- liftIO $ hIsEOF p
  return $ Boolean eof
eofObjectp args = return $ Boolean False

charReadyp :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
charReadyp []        = charReadyp [Port stdin]
charReadyp [Port p]  = do 
  ready <- liftIO $ hReady p
  return $ Boolean ready
charReadyp [notPort] = throwError $ TypeMismatch "port" notPort
charReadyp args      = throwError $ NumArgs 1 args

writeChar :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
writeChar [Char c]              = writeChar [Char c, Port stdout]
writeChar [Char c, Port p]      = liftIO $ hPutChar p c >> (return $ Boolean True)
writeChar [notChar@(Char _), _] = throwError $ TypeMismatch "character" notChar
writeChar [_, notPort]          = throwError $ TypeMismatch "port" notPort
writeChar args                  = throwError $ NumArgs 2 args

writeProc :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Boolean True)
writeProc [obj, notPort]   = throwError $ TypeMismatch "port" notPort
writeProc args             = throwError $ NumArgs 2 args

loadProc env [String filename] = load env filename
loadProc env [x]               = throwError $ TypeMismatch "string" x
loadProc env xs                = throwError $ NumArgs 1 xs

load :: SchemeEnvironment -> String -> IOThrowsSchemeError SchemeValue
load env filename = do
  program <- liftIO $ readFile filename
  tokens  <- liftIOScanner $ scan program
  ast     <- liftParser  $ parse tokens
  eval env ast
