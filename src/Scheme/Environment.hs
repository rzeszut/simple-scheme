module Scheme.Environment ( Environment(..)
                          , nullEnvironment
                          , isBound
                          , getVar
                          , setVar
                          , defineVar
                          , bindVars
                          , newEnv
                          ) where

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.IORef
import Scheme.Error

data Environment a = Environment { parentEnv :: Maybe (Environment a)
                                 , bindings  :: IORef [(String, IORef a)]
                                 }

nullEnvironment :: IO (Environment a)
nullEnvironment = do bindings <- newIORef []
                     return $ Environment Nothing bindings

isBound :: Environment a -> String -> IO Bool
isBound env var = do
  binds <- liftIO $ readIORef $ bindings env
  case lookup var binds of
    Just _  -> return True
    Nothing -> case parentEnv env of
      Just par -> isBound par var
      Nothing  -> return False

getVar :: Environment a -> String -> IOThrowsError a a
getVar env var = do
  binds <- liftIO $ readIORef $ bindings env
  case lookup var binds of
    Just ref -> liftIO $ readIORef ref
    Nothing  -> case parentEnv env of
      Just par -> getVar par var
      Nothing  -> throwError $ UnboundVar "Unbound variable" var

setVar :: Environment a -> String -> a -> IOThrowsError a a
setVar env var val = do
  binds <- liftIO $ readIORef $ bindings env
  case lookup var binds of
    Just ref -> (liftIO $ writeIORef ref val) >> return val
    Nothing  -> case parentEnv env of
      Just par -> setVar par var val
      Nothing  -> throwError $ UnboundVar "Setting unbound variable" var

defineVar :: Environment a -> String -> a -> IOThrowsError a a
defineVar env var val = do
  binds <- liftIO $ readIORef $ bindings env
  case lookup var binds of
    Just ref -> (liftIO $ writeIORef ref val) >> return val
    Nothing  -> liftIO $ do
      valRef <- newIORef val
      writeIORef (bindings env) ((var, valRef) : binds)
      return val

bindVars :: Environment a -> [(String, a)] -> IO (Environment a)
bindVars envRef newBindings = do
  bindinglist <- mapM (\(name, val) ->
                        do ref <- newIORef val
                           return (name, ref)) newBindings
                 >>= newIORef
  return $ Environment (Just envRef) bindinglist

newEnv :: Environment a -> IO (Environment a)
newEnv envRef = do binds <- newIORef []
                   return $ Environment (Just envRef) binds
