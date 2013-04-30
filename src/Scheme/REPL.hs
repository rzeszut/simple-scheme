module Scheme.REPL ( runOne
                   , runRepl
                   ) where

import Control.Monad
import Scheme.Data
import Scheme.Error
import Scheme.Eval
import Scheme.Environment
import Scheme.Parser
import System.IO

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt str = flushStr str >> getLine

evalString :: Environment -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Environment -> String -> IO ()
evalAndPrint env str = evalString env str >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = nullEnvironment >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnvironment >>= until_ (== ":q") (readPrompt "scheme> ") . evalAndPrint
