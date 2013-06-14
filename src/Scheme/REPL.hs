module Scheme.REPL ( runFile
                   , runRepl
                   ) where

import Control.Monad
import Paths_simple_scheme (getDataFileName)
import Scheme.Data
import Scheme.Eval
import Scheme.Parser (parse)
import Scheme.Primitives
import Scheme.Primitives.IO (load)
import Scheme.Scanner (scan)
import System.IO
import Scheme.Error (runIOThrows, liftThrows, liftIOScanner, liftParser)
import Scheme.Environment (bindVars, defineVar)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: SchemeEnvironment -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
                      (liftIOScanner $ scan expr)
                      >>= (\ast -> liftParser $ parse ast)
                      >>= eval env

evalAndPrint :: SchemeEnvironment -> String -> IO ()
evalAndPrint env str
    | length str == 0 = return ()
    | otherwise      = evalString env str >>= putStrLn

ioUntil :: (String -> Bool) -> String -> (String -> IO ()) -> IO ()
ioUntil pred prompt action = do
  flushStr prompt
  eof <- isEOF
  if eof then return ()
    else do
    result <- getLine
    if pred result
      then return ()
      else action result >> ioUntil pred prompt action

r5rsEnvironment :: IO SchemeEnvironment
r5rsEnvironment = do
  env <- primitiveBindings
  stdlib <- getDataFileName "scheme/stdlib.scm"
  runIOThrows $ liftM show $ load env stdlib
  return env

runFile :: [String] -> IO ()
runFile args = do
  env <- r5rsEnvironment >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ load env $ args !! 0) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
  env <- r5rsEnvironment
  ioUntil (== ":q") "scheme> " . evalAndPrint $ env
