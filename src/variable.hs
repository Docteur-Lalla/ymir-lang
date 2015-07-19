module Variable where
import Value
import Error
import Control.Monad.Error
import Data.IORef

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound env var = readIORef env >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError YmirValue
getVar envRef var =
  do
    env <- liftIO $ readIORef envRef
    maybe error (liftIO . readIORef) (lookup var env)

    where error = throwError (UnboundVariable "Getting an unbound variable" var)

setVar :: Env -> String -> YmirValue -> IOThrowsError YmirValue
setVar envRef var value =
  do
    env <- liftIO $ readIORef envRef
    maybe error (liftIO . (flip writeIORef value)) (lookup var env)
    return value
  
    where error = throwError (UnboundVariable "Setting an unbound variable" var)

defineVar :: Env -> String -> YmirValue -> IOThrowsError YmirValue
defineVar envRef var value =
  do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
      then setVar envRef var value >> return value
      else liftIO $ do
        valueRef <- newIORef value
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value

bindVars :: Env -> [(String, YmirValue)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) =
      do
        ref <- newIORef value
        return (var, ref)
