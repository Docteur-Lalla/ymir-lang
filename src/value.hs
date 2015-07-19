module Value where

data YmirValue = Atom String
  | List [YmirValue]
  | DottedList [YmirValue] YmirValue
  | Number Integer
  | Char Char
  | String String
  | Bool Bool
