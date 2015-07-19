module Eval where
import Value
import Error
import Control.Monad.Error
import Primitives
import Variable

eval :: Env -> YmirValue -> IOThrowsError YmirValue
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "define":List (Atom var:params):body)) = function
  where function = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params) varargs:body)) = function
  where  function = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda":List params:body)) = makeNormalFunc env params body
eval env (List (Atom "lambda":DottedList params varargs:body)) = lambda
  where lambda = makeVarargs varargs env params body
eval env (List (Atom "lambda":varargs@(Atom _):body)) = lambda
  where lambda = makeVarargs varargs env [] body
eval env (List (f:args)) =
  do
    func <- eval env f
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

makeFunc varargs env params body = return $ Closure pnames varargs body env

  where pnames = map showValue params

makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showValue

apply :: YmirValue -> [YmirValue] -> IOThrowsError YmirValue
apply (Primitive f) args = liftThrows $ f args
apply (Closure params varargs body env) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars env $ zip params args) >>=
      bindVarArgs varargs >>= evalBody

  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
      Nothing -> return env
