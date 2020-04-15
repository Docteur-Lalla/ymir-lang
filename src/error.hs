module Error where
import Value
import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
