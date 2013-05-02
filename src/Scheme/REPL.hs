module Scheme.REPL ( runFile
                   , runRepl
                   ) where

import Control.Monad
import Scheme.Data
import Scheme.Eval
import Scheme.Parser
import Scheme.Primitives
import System.IO
import Lang.Utils.Error (runIOThrows, liftThrows)
import Lang.Utils.Environment (bindVars)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt str = flushStr str >> getLine

evalString :: SchemeEnvironment -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: SchemeEnvironment -> String -> IO ()
evalAndPrint env str = evalString env str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runFile :: [String] -> IO ()
runFile args = do
  env <- primitiveBindings >>= flip bindVars [("args", toLispList $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (Cons (Symbol "load") (Cons (String $ args !! 0) Nil)))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== ":q") (readPrompt "scheme> ") . evalAndPrint
