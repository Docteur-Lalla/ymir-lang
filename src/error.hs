module Error where
import Value
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

data YmirError = NumArgs Integer [YmirValue]
  | TypeMismatch String YmirValue
  | Parser ParseError
  | BadSpecialForm String YmirValue
  | NotFunction String String
  | UnboundVariable String String
  | Default String

instance Error YmirError where
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either YmirError

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
