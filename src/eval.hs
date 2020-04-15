module Eval where
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
eval badForm = failWith $ BadSpecialForm "Unrecognized special form" badForm
