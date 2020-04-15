module Environment where
import Control.Monad.Except
import Data.Maybe (isJust)
import Value hiding (Env)

type Env = [(String, YmirValue)]

nullEnv :: Env
nullEnv = []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> Bool
isBound env var = isJust $ lookup var env

getVar :: Env -> String -> ThrowsError YmirValue
getVar env var = case lookup var env of
                    Nothing -> error
                    Just val -> return val
  where error = throwError (UnboundVariable "Getting an unbound variable" var)

setVar :: Env -> String -> YmirValue -> ThrowsError Env
setVar env var val =
  let (exists, res) = foldr modify (False, []) env in
  if exists then return res else error
  where modify p (True, lst) = (True, p:lst)
        modify old@(name, _) (False, lst)
          | name == var = (True, (var, val):lst)
          | otherwise = (False, old:lst)

        error = throwError (UnboundVariable "Setting an unbound variable" var)

defineVar :: Env -> String -> YmirValue -> ThrowsError Env
defineVar env var val
  | isBound env var = setVar env var val
  | otherwise = return $ (var, val):env
