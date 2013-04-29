import System.Environment

import Control.Monad
import Scheme.Data
import Scheme.Error
import Scheme.Parser
import Scheme.Eval

main :: IO ()
main = do
  args <- getArgs
  evaled <- return . liftM show $ readExpr (args !! 0) >>= eval
  putStrLn . extractValue . trapError $ evaled
