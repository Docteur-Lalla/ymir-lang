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
eval env (List (Atom f : args)) = mapM (eval env) args >>= liftThrows . apply f
eval env badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

apply :: String -> [YmirValue] -> ThrowsError YmirValue
apply f args = maybe error ($ args) (lookup f primitives)
  where error = throwError (NotFunction "Unrecognized primitive function args" f)
