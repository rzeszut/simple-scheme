module Scheme.REPL ( runOne
                   , runRepl
                   ) where

import Control.Monad
import Scheme.Data
import Scheme.Eval
import Scheme.Parser
import Scheme.Primitives
import System.IO
import Lang.Utils.Error (runIOThrows, liftThrows)
import Lang.Utils.Environment (nullEnvironment)

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

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== ":q") (readPrompt "scheme> ") . evalAndPrint
