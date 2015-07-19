module Eval where
import Value
import Error
import Control.Monad.Error
import Primitives

eval :: YmirValue -> ThrowsError YmirValue
eval val@(Atom _) = return val
eval val@(String _) = return val
eval val@(Char _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

apply :: String -> [YmirValue] -> ThrowsError YmirValue
apply f args = maybe error ($ args) (lookup f primitives)
  where error = throwError (NotFunction "Unrecognized primitive function args" f)
