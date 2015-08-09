module Function
  (makeArgument,
  makeFunc,
  makeNormalFunc,
  makeVarargs,
  makeMacro,
  makeNormalMacro,
  makeVarargsMacro,
  applyProc,
  apply) where

import Value
import Error
import Variable
import Control.Monad.Error

type EvalFunction = Env -> YmirValue -> IOThrowsError YmirValue

makeArgument :: Env -> YmirValue -> (String, Bool)
makeArgument env (Atom name) = (name, False)
makeArgument env (List [Atom "quote", Atom name]) = (name, True)
makeArgument env value = ("", False)

makeFunc :: Maybe (String, Bool) -> Env -> [YmirValue] -> [YmirValue] -> IOThrowsError YmirValue
makeFunc varargs env params body = return $ Closure pnames varargs body env
  where pnames = map (makeArgument env) params

makeNormalFunc = makeFunc Nothing
makeVarargs varargs env = makeFunc (Just $ makeArgument env varargs) env

makeMacro :: Maybe (String, Bool) -> Env -> [YmirValue] -> [YmirValue] -> IOThrowsError YmirValue
makeMacro varargs env params body = return $ Macro pnames varargs body
  where pnames = map (makeArgument env) params

makeNormalMacro = makeMacro Nothing
makeVarargsMacro varargs env = makeMacro (Just $ makeArgument env varargs) env

applyProc :: Env -> EvalFunction -> [YmirValue] -> IOThrowsError YmirValue
applyProc env eval [f, List args] = apply env eval f args
applyProc env eval (f:args) = apply env eval f args

apply :: Env -> EvalFunction -> YmirValue -> [YmirValue] -> IOThrowsError YmirValue
apply _ _ (Primitive f) args = liftThrows $ f args
apply _ eval (Closure paramPair varargPair body env) args =
  if num (makeParams paramPair) /= num args && varargs (varargPair) == Nothing
    then throwError $ NumArgs (num $ makeParams paramPair) args
    else (liftIO $ bindVars env $ zip (makeParams paramPair) args) >>=
      bindVarArgs (remainingArgs paramPair args) (varargs varargPair) >>= evalBody eval body

makeParams paramPair = map fst paramPair
varargs Nothing = Nothing
varargs (Just (name, b)) = Just name
remainingArgs paramPair args = drop (length $ makeParams paramPair) args

num = toInteger . length

evalBody :: EvalFunction -> [YmirValue] -> Env -> IOThrowsError YmirValue
evalBody eval body env = liftM last $ mapM (eval env) body
bindVarArgs remainingArgs arg env = case arg of
  Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
  Nothing -> return env
