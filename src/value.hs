module Value where
import Data.IORef
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef YmirValue)]

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

data YmirValue = Atom String
  | List [YmirValue]
  | DottedList [YmirValue] YmirValue
  | Number Integer
  | Char Char
  | String String
  | Bool Bool
  | Primitive ([YmirValue] -> ThrowsError YmirValue)
  | Closure
  {
    params :: [String],
    vararg :: (Maybe String),
    body :: [YmirValue],
    closure :: Env
  }

showValue :: YmirValue -> String
showValue (String s) = "\"" ++ s ++ "\""
showValue (Char c) = "'" ++ [c] ++ "'"
showValue (Atom a) = a
showValue (Number n) = show n
showValue (Bool True) = "true"
showValue (Bool False) = "false"
showValue (List li) = "(" ++ unwordsList li ++ ")"
showValue (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showValue t ++ ")"
showValue (Primitive _) = "<primitive>"
showValue (Closure {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
    Nothing -> ""
    Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList :: [YmirValue] -> String
unwordsList = unwords . map showValue

instance Show YmirValue where show = showValue
