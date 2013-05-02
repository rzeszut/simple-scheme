import System.Environment (getArgs)
import Scheme.REPL (runFile, runRepl)

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else (runFile args)
