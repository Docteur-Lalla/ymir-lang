module Function
  (makeArgument,
  makeFunc,
  makeNormalFunc,
  makeVarargs,
  makeMacro,
  makeNormalMacro,
  makeVarargsMacro,
  applyProc,
  applyFunction,
  apply) where

import Value
import Error
import Variable
import Control.Monad.Error
import System.IO.Unsafe

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

applyFunction :: EvalFunction -> YmirValue -> [YmirValue] -> IOThrowsError YmirValue
applyFunction _ (Primitive f) args = liftThrows $ f args
applyFunction eval (Closure paramPair varargPair body env) args =
  if num (makeParams paramPair) /= num args && varargs (varargPair) == Nothing
    then throwError $ NumArgs (num $ makeParams paramPair) args
    else (liftIO $ bindVars env $ zip (makeParams paramPair) args) >>=
      bindVarArgs (remainingArgs paramPair args) (varargs varargPair) >>= evalBody eval body

apply :: Env -> EvalFunction -> YmirValue -> [YmirValue] -> IOThrowsError YmirValue
apply _ eval f@(Primitive _) args = applyFunction eval f args
apply _ eval f@(Closure _ _ _ _) args = applyFunction eval f args
apply env eval (Macro paramPair varargPair body) args =
  if num (makeParams paramPair) /= num args && varargs (varargPair) == Nothing
    then throwError $ NumArgs (num $ makeParams paramPair) args
    else liftM last $ mapM (eval env) res

  where
    replaceEach [] var = var
    replaceEach (x:xs) var = replaceEach xs (replaceOccurence x var)

    res = map (replaceEach arguments) body
    arguments = named ++ variables
    named = zip (makeParams paramPair) args
    variables = case (varargs varargPair) of
      Nothing -> []
      Just name -> [(name, List $ remainingArgs paramPair args)]

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

replaceOccurence :: (String, YmirValue) -> YmirValue -> YmirValue
replaceOccurence (arg, Atom a') (Atom a)
  | a == arg = List [Atom "quote", Atom a']
  | otherwise = Atom a
replaceOccurence (arg, val) (Atom a)
  | a == arg = val
  | otherwise = Atom a
replaceOccurence (arg, val) (List li) = List $ map (replaceOccurence (arg, val)) li
replaceOccurence (arg, val) (DottedList x xs) =
  DottedList (map (replaceOccurence (arg, val)) x) (replaceOccurence (arg, val) xs)
replaceOccurence (_, _) var = var
