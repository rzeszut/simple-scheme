import System.Environment

import Control.Monad
import Scheme.REPL
import System.IO

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else (runFile args)
