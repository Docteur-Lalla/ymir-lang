module Value where

data YmirValue = Atom String
  | List [YmirValue]
  | DottedList [YmirValue] YmirValue
  | Number Integer
  | Char Char
  | String String
  | Bool Bool

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
