module Lang.Utils.Environment ( Environment(..)
                                , nullEnvironment
                                , getVar
                                , setVar
                                , defineVar
                                , bindVars
                                ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Lang.Utils.Error

type Environment a = IORef [(String, IORef a)]

nullEnvironment :: IO (Environment a)
nullEnvironment = newIORef []

isBound :: Environment a -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Environment a -> String -> IOThrowsError a a
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Environment a -> String -> a -> IOThrowsError a a
setVar envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting unbound variable" var)
    (liftIO . (flip writeIORef val))
    (lookup var env)
  return val

defineVar :: Environment a -> String -> a -> IOThrowsError a a
defineVar envRef var val = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var val >> return val
    else liftIO $ do
    valRef <- newIORef val
    env <- readIORef envRef
    writeIORef envRef ((var, valRef) : env)
    return val

bindVars :: Environment a -> [(String, a)] -> IO (Environment a)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, val)  = do ref <- newIORef val
                                return (var, ref)

