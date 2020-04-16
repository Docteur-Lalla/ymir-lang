module Function
  (makeArgument,
  makeFunc,
  makeNormalFunc,
  makeVarargs,
  makeMacro,
  makeNormalMacro,
  makeVarargsMacro,
  applyFunction,
  apply) where

import Control.Monad.Except
import Data.Maybe (isNothing, maybeToList)
import Environment
import Error
import Interpreter
import System.IO.Unsafe
import Value

-- |Make a function argument from the given value.
-- |If the value is quoted, then the argument shall not be evaluated before
-- |being sent to the function.
makeArgument :: Env -> YmirValue -> Parameter
makeArgument env (Atom name) = (name, False)
makeArgument env (List [Atom "quote", Atom name]) = (name, True)
makeArgument env value = ("", False)

-- |Create a function from the given variadic parameter name, environment,
-- |symbol list and body.
makeFunc :: Maybe Parameter -> Env -> [YmirValue] -> [YmirValue] -> YmirValue
makeFunc varargs env params body = Closure pnames varargs body env
  where pnames = map (makeArgument env) params

makeNormalFunc = makeFunc Nothing
makeVarargs varargs env = makeFunc (Just $ makeArgument env varargs) env

-- |Make a macro from the same informations as @makeFunc.
makeMacro :: Maybe Parameter -> Env -> [YmirValue] -> [YmirValue] -> YmirValue
makeMacro varargs env params = Macro pnames varargs
  where pnames = map (makeArgument env) params

makeNormalMacro = makeMacro Nothing
makeVarargsMacro varargs env = makeMacro (Just $ makeArgument env varargs) env

type EvalFunction = YmirValue -> Interp Env YmirValue

-- |Apply the given function to the argument list. This function is the
-- |equivalent of the splat operator of other languages.
applyProc :: Env -> EvalFunction -> [YmirValue] -> Interp Env YmirValue
applyProc env eval [f, List args] = apply env eval f args
applyProc env eval (f:args) = apply env eval f args

-- |Apply the given function to the given arguments.
applyFunction :: EvalFunction -> YmirValue -> [YmirValue] -> Interp Env YmirValue
applyFunction _ (Primitive f) args = Interp $ \e -> liftThrows $ fmap ((,) e) (f args)
applyFunction eval (Closure paramPairs varargPair body env) args =
  withRightParameterCount paramPairs args varargPair $ do
    let env2 = bindVars env $ zip (makeParams paramPairs) args
    env3 <- Interp $ \_ -> do
      e <- liftThrows env2 >>= bindVarArgs (remainingArgs paramPairs args) (varargs varargPair)
      return (e, e)
    evalBody eval body env3

-- |Apply the function, primitive or macro to the given arguments.
apply :: Env -> EvalFunction -> YmirValue -> [YmirValue] -> Interp Env YmirValue
apply _ eval f@(Primitive _) args = applyFunction eval f args
apply _ eval f@Closure {} args = applyFunction eval f args
apply env eval (Macro paramPairs varargPair body) args =
  withRightParameterCount paramPairs args varargPair (evalBody eval res env)
  where
    res = map (replaceEach $ named ++ variables) body
    replaceEach lst var = foldl (flip replaceOccurence) var lst
    -- Bind the parameter to the function argument names.
    named = zip (makeParams paramPairs) args
    -- Bind the variadic parameter if any.
    variables = fmap makeVarargParam $ maybeToList $ varargs varargPair
    makeVarargParam name = (name, List $ remainingArgs paramPairs args)

withRightParameterCount :: [Parameter] -> [YmirValue] -> Maybe Parameter -> Interp Env a -> Interp Env a
withRightParameterCount paramPairs args varargPair f =
  if num (makeParams paramPairs) /= num args && isNothing (varargs varargPair)
    then failWith $ NumArgs (num $ makeParams paramPairs) args
    else f

makeParams = map fst
varargs Nothing = Nothing
varargs (Just (name, b)) = Just name
remainingArgs paramPair = drop (length $ makeParams paramPair)

num = toInteger . length

evalBody :: EvalFunction -> [YmirValue] -> Env -> Interp Env YmirValue
evalBody eval body env = Interp $ \e -> do
  pairs <- mapM (flip runInterp env . eval) body
  let vals = fmap snd pairs
  return (e, last vals)

bindVarArgs remainingArgs arg env = case arg of
  Just argName -> liftThrows $ bindVars env [(argName, List remainingArgs)]
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
