module Function
  (apply,
  applyFunction,
  applyProc,
  differentLengths,
  makeArgument,
  makeFunc,
  makeMacro,
  makeNormalFunc,
  makeNormalMacro,
  makeVarargs,
  makeVarargsMacro,
  remainingArguments) where

import Control.Monad.Except
import Data.IORef
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
makeFunc varargs env params body = Closure pnames varargs body envRef
  where pnames = map (makeArgument env) params
        envRef = unsafePerformIO $ newIORef env

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
applyFunction eval (Closure paramPairs varargPair body envRef) args =
  withRightParameterCount paramPairs args varargPair $ do
    env <- Interp $ \e -> (,) e <$> lift (readIORef envRef)
    let env2 = bindVars env $ bindParameters paramPairs args
    env3 <- Interp $ \_ -> do
      e <- liftThrows env2 >>= bindVarArgs (remainingArguments paramPairs args) (varargName varargPair)
      return (e, e)
    evalBody eval body env3

-- |Apply the function, primitive or macro to the given arguments.
apply :: Env -> EvalFunction -> YmirValue -> [YmirValue] -> Interp Env YmirValue
apply _ eval f@(Primitive _) args = applyFunction eval f args
apply _ eval f@Closure {} args = do
  env <- environment
  val <- applyFunction eval f args
  setEnvironment env
  return val
apply env eval (Macro paramPairs varargPair body) args =
  withRightParameterCount paramPairs args varargPair (evalBody eval res env)
  where
    res = map (replaceEach $ named ++ variables) body
    replaceEach lst var = foldl (flip replaceOccurence) var lst
    -- Bind the parameter to the function argument names.
    named = bindParameters paramPairs args
    -- Bind the variadic parameter if any.
    variables = fmap makeVarargParam $ maybeToList $ varargName varargPair
    makeVarargParam name = (name, List $ remainingArguments paramPairs args)

-- |Return the given interpreter iff the the number of arguments sent to the
-- |function matches the number of expected parameter or if there are enough
-- |arguments sent to a variadic function.
withRightParameterCount :: [Parameter] -> [YmirValue] -> Maybe Parameter -> Interp Env a -> Interp Env a
withRightParameterCount paramPairs args varargPair f
  | differentLengths paramPairs args && isNothing varargPair = failWith $ NumArgs (num paramPairs) args
  | otherwise = f
  where num = toInteger . length

-- |Test if the length of both lists is different.
differentLengths :: [a] -> [b] -> Bool
differentLengths list1 list2 = length list1 /= length list2

-- |Bind the parameter name with the provided value.
bindParameters :: [(String, Bool)] -> [YmirValue] -> [(String, YmirValue)]
bindParameters paramPairs = zip (map fst paramPairs)

-- |Extract the name of a variadic parameter if any.
varargName :: Maybe Parameter -> Maybe String
varargName = fmap fst

-- |Get the list of arguments sent to a function that have not been bound to a
-- |positional parameter.
remainingArguments :: [Parameter] -> [a] -> [a]
remainingArguments paramPairs = drop (length paramPairs)

-- |Evaluate a body of multiple statement and return the value computed by the
-- |last one.
evalBody :: EvalFunction -> [YmirValue] -> Env -> Interp Env YmirValue
evalBody eval (x:rest) env = Interp $ \e -> do
  fst <- runInterp (eval x) env
  (_, val) <- evalFold fst rest
  return (e, val)
    where evalFold = foldM (\(env, _) val -> flip runInterp env $! eval val)

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
