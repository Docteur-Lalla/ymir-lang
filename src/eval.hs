module Eval where
import Control.Monad.Except
import Data.Maybe (isNothing)
import Environment
import Function
import Interpreter
import Value

eval :: YmirValue -> Interp Env YmirValue
eval val@(String _) = return val
eval val@(Char _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id) = readVar id
eval (DottedList x xs) = do
  x' <- mapM eval x
  xs' <- eval xs
  return $ DottedList x' xs'
eval (List [Atom "define", Atom var, form]) = do
  val <- eval form
  defineNewVar var val
  return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom "define":List (Atom var:params):body)) = do
  env <- environment
  let function = makeNormalFunc env params body
  defineNewVar var function
  return function
eval (List (Atom "define":DottedList (Atom var:params) varargs:body)) = do
  env <- environment
  let function = makeVarargs varargs env params body
  defineNewVar var function
  return function
eval (List (Atom "def-macro":List (Atom var:params):body)) = do
  env <- environment
  let macro = makeNormalMacro env params body
  defineNewVar var macro
  return macro
eval (List (Atom "def-macro":DottedList (Atom var:params) varargs:body)) = do
  env <- environment
  let macro = makeVarargsMacro varargs env params body
  defineNewVar var macro
  return macro
eval (List (Atom "lambda":List params:body)) = do
  env <- environment
  return $ makeNormalFunc env params body
eval (List (Atom "lambda":DottedList params varargs:body)) = do
  env <- environment
  return $ makeVarargs varargs env params body
eval (List (Atom "lambda":varargs@(Atom _):body)) = do
  env <- environment
  return $ makeVarargs varargs env [] body
eval (List (f:args)) = do
  env <- environment
  func <- eval f
  case func of
    Closure params varargs body ctx -> do
      let computeArgM = mapM computeArg
      argVals <- computeArgM (zipped params args)
      varArgVals <- computeArgM $ zippedVarArgs params args varargs
      if num args /= num params && isNothing varargs
        then do
          dropped <- mapM eval (drop (length params) args)
          failWith $ NumArgs (num params) (argVals ++ dropped)
        else apply env eval func (argVals ++ varArgVals)

    Primitive _ -> do
      argVals <- mapM eval args
      apply env eval func argVals
    Macro params varargs body -> do
      argVals <- mapM computeArg (zipped params args)
      varArgVals <- mapM computeArg $ zippedVarArgs params args varargs
      if num args /= num params && isNothing varargs
        then do
          dropped <- mapM eval (drop (length params) args)
          failWith $ NumArgs (num params) (argVals ++ dropped)
        else apply env eval func (argVals ++ varArgVals)
    _ -> failWith (NotFunction "Given value is not a function" $ show f)
  where
    num = toInteger . length
    computeArg ((_, True), value) = eval (List [Atom "quote", value])
    computeArg ((_, False), value) = eval value

    zipped params args = zip params (take (length params) args)
    varArgs _ _ Nothing = []
    varArgs params args (Just _) = drop (length params) args
    zippedVarArgs _ _ Nothing = []
    zippedVarArgs params args (varargs@(Just (name, b))) =
      map (\var -> ((name, b), var)) (varArgs params args varargs)

eval badForm = failWith $ BadSpecialForm "Unrecognized special form" badForm
