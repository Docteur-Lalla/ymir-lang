module Eval where
import Value
import Primitives
import Control.Monad.Error

showValue :: YmirValue -> String
showValue (String s) = "\"" ++ s ++ "\""
showValue (Char c) = "'" ++ [c] ++ "'"
showValue (Atom a) = a
showValue (Number n) = show n
showValue (Bool True) = "true"
showValue (Bool False) = "false"
showValue (List li) = "(" ++ unwordsList li ++ ")"
showValue (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showValue t ++ ")"

unwordsList :: [YmirValue] -> String
unwordsList = unwords . map showValue

instance Show YmirValue where show = showValue

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

showError :: YmirError -> String
showError (UnboundVariable message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message f) = message ++ ": " ++ show f
showError (NumArgs exp found) = "Expected " ++ show exp
  ++ " arguments: found values " ++ unwordsList found
showError (TypeMismatch exp found) = "Invalid type: expected " ++ exp
  ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show YmirError where show = showError
