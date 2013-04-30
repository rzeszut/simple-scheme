module Scheme.Environment where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Scheme.Data
import Scheme.Error

type Environment = IORef [(String, IORef SchemeValue)]

nullEnvironment :: IO Environment
nullEnvironment = newIORef []

isBound :: Environment -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Environment -> String -> IOThrowsError SchemeValue
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Environment -> String -> SchemeValue -> IOThrowsError SchemeValue
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting unbound variable" var)
    (liftIO . (flip writeIORef val))
    (lookup var env)
  return val

defineVar :: Environment -> String -> SchemeValue -> IOThrowsError SchemeValue
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val >> return val
    else liftIO $ do
    valRef <- newIORef val
    env <- readIORef envRef
    writeIORef envRef ((var, valRef) : env)
    return val

bindVars :: Environment -> [(String, SchemeValue)] -> IO Environment
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, val)  = do ref <- newIORef val
                                return (var, ref)

