module Scheme.Primitives.IO (ioPrimitives) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Lang.Utils.Error
import Scheme.Data
import System.IO
import Scheme.Parser

ioPrimitives :: [(String, [SchemeValue] -> IOThrowsSchemeError SchemeValue)]
ioPrimitives = [ ("open-input-file", makePort ReadMode)
               , ("open-output-file", makePort WriteMode)
               , ("close-input-port", closePort)
               , ("close-output-port", closePort)
               , ("read", readProc)
               , ("write", writeProc)
               , ("read-contents", readContents)
--               , ("read-all", readAll)
               ]

makePort :: IOMode -> [SchemeValue] -> IOThrowsSchemeError SchemeValue
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [x]                  = throwError $ TypeMismatch "string" x
makePort _ xs                   = throwError $ NumArgs 1 xs

closePort :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
closePort [Port port] = liftIO $ hClose port >> (return $ Boolean True)
closePort [x]         = throwError $ TypeMismatch "port" x
closePort xs          = throwError $ NumArgs 1 xs

-- TODO: errors
readProc :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr 

-- TODO: errors
writeProc :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Boolean True)

-- TODO: errors
readContents :: [SchemeValue] -> IOThrowsSchemeError SchemeValue
readContents [String filename] = liftM String $ liftIO $ readFile filename
