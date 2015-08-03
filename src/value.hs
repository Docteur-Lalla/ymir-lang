module Value where
import Foreign.Ptr
import Data.IORef
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type Env = IORef [(String, IORef YmirValue)]

data Number = Integer Int
  | Float Double
  deriving (Eq, Ord, Read)

instance Show Number where
  show (Integer i) = show i
  show (Float f) = show f

instance Fractional Number where
  (/) (Integer a) (Integer b) = (Integer $ a `div` b)
  (/) (Integer a) (Float b) = (Float $ fromIntegral a / b)
  (/) (Float a) (Integer b) = (Float $ a / fromIntegral b)
  (/) (Float a) (Float b) = (Float $ a / b)

instance Num Number where
  (+) (Integer a) (Integer b) = (Integer $ a + b)
  (+) (Float a) (Integer b) = (Float $ a + fromIntegral b)
  (+) (Integer a) (Float b) = (Float $ fromIntegral  a + b)
  (+) (Float a) (Float b) = (Float $ a + b)

  (*) (Integer a) (Integer b) = (Integer $ a * b)
  (*) (Float a) (Integer b) = (Float $ a * fromIntegral b)
  (*) (Integer a) (Float b) = (Float $ fromIntegral a * b)
  (*) (Float a) (Float b) = (Float $ a * b)

  negate (Integer i) = (Integer $ -i)
  negate (Float f) = (Float $ -f)

  abs (Integer i)
    | i < 0 = Integer (-i)
    | otherwise = Integer i
  abs (Float f)
    | f < 0 = Float (-f)
    | otherwise = Float f

  signum (Integer i)
    | i < 0 = Integer (-1)
    | i == 0 = Integer 0
    | otherwise = Integer 1
  signum (Float f)
    | f < 0 = Float (-1)
    | f == 0 = Float 0
    | otherwise = Float 1

  fromInteger i = Integer (fromIntegral i)

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
type IOThrowsError = ErrorT YmirError IO

data YmirValue = Atom String
  | List [YmirValue]
  | DottedList [YmirValue] YmirValue
  | Number Number
  | Char Char
  | String String
  | Bool Bool
  | Pointer (Ptr ())
  | Primitive ([YmirValue] -> ThrowsError YmirValue)
  | Closure
  {
    params :: [(String, Bool)],
    vararg :: (Maybe (String, Bool)),
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
showValue (Pointer ptr) = "<pointer " ++ show ptr ++ ">"
showValue (Primitive _) = "<primitive>"
showValue (Closure {params = arguments, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
    Nothing -> ""
    Just (arg, b) -> " . " ++ (if b then "'" ++ arg else arg)) ++ ") ...)"

  where args = map (\(str, b) -> if b then "'" ++ str else str) arguments

unwordsList :: [YmirValue] -> String
unwordsList = unwords . map showValue

instance Show YmirValue where show = showValue
