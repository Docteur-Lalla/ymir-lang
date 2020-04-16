module Value where
import Control.Monad.Except
import Data.IORef
import Data.Ratio (numerator, denominator)
import Foreign.Ptr
import Text.ParserCombinators.Parsec (ParseError)

type Env = [(String, YmirValue)]

data Number = Integer Int
  | Float Double
  deriving (Eq, Ord, Read)

instance Show Number where
  show (Integer i) = show i
  show (Float f) = show f

instance Fractional Number where
  (/) (Integer a) (Integer b) = Integer $ a `div` b
  (/) (Integer a) (Float b) = Float $ fromIntegral a / b
  (/) (Float a) (Integer b) = Float $ a / fromIntegral b
  (/) (Float a) (Float b) = Float $ a / b

  fromRational r
    | denominator r == 1 = Integer $ fromIntegral $ numerator r
    | otherwise = Float $ fromRational r

instance Num Number where
  (+) (Integer a) (Integer b) = Integer $ a + b
  (+) (Float a) (Integer b) = Float $ a + fromIntegral b
  (+) (Integer a) (Float b) = Float $ fromIntegral  a + b
  (+) (Float a) (Float b) = Float $ a + b

  (*) (Integer a) (Integer b) = Integer $ a * b
  (*) (Float a) (Integer b) = Float $ a * fromIntegral b
  (*) (Integer a) (Float b) = Float $ fromIntegral a * b
  (*) (Float a) (Float b) = Float $ a * b

  negate (Integer i) = Integer $ -i
  negate (Float f) = Float $ -f

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

type Parameter = (String, Bool)
type ThrowsError = Either YmirError
type IOThrowsError = ExceptT YmirError IO

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
    params :: [Parameter],
    vararg :: Maybe Parameter,
    body :: [YmirValue],
    closure :: Env
  }
  | Macro
  {
    params :: [Parameter],
    vararg :: Maybe Parameter,
    body :: [YmirValue]
  }

instance Show YmirValue where
  show (String s) = "\"" ++ s ++ "\""
  show (Char c) = "'" ++ [c] ++ "'"
  show (Atom a) = a
  show (Number n) = show n
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (List li) = "(" ++ unwordsList li ++ ")"
  show (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
  show (Pointer ptr) = "<pointer " ++ show ptr ++ ">"
  show (Primitive _) = "<primitive>"
  show Closure {params = arguments, vararg = varargs, body = body, closure = env} =
    let argsString = stringFromArguments arguments
        varargsString = stringFromVarargs varargs in
    "(lambda (" ++ argsString ++ varargsString ++ ") ...)"
  show Macro {params = arguments, vararg = varargs, body = body} =
    let argsString = stringFromArguments arguments
        varargsString = stringFromVarargs varargs in
    "(macro (" ++ argsString ++ varargsString ++ ") ...)"

stringFromArguments :: [Parameter] -> String
stringFromArguments = unwords . map show . args
  where args = map (\(str, b) -> if b then "'" ++ str else str)

stringFromVarargs :: Maybe Parameter -> String
stringFromVarargs Nothing = ""
stringFromVarargs (Just (arg, b)) = " . " ++ (if b then "'" ++ arg else arg)

unwordsList :: [YmirValue] -> String
unwordsList = unwords . map show

data YmirType = IntegerType
  | FloatType
  | NumberType
  | ListType
  | DottedListType
  | StringType
  | CharType
  | FunctionType
  | MacroType
  | BoolType
  | PointerType
  | SymbolType
  deriving (Eq)

class VariableType a where
  typeOf :: YmirValue -> a
  is_a :: YmirValue -> a -> Bool

instance VariableType YmirType where
  typeOf (Atom _) = SymbolType
  typeOf (Number (Integer _)) = IntegerType
  typeOf (Number (Float _)) = FloatType
  typeOf (List _) = ListType
  typeOf (DottedList _ _) = DottedListType
  typeOf (String _) = StringType
  typeOf (Char _) = CharType
  typeOf (Primitive _) = FunctionType
  typeOf Closure {} = FunctionType
  typeOf Macro {} = MacroType
  typeOf (Bool _) = BoolType
  typeOf (Pointer _) = PointerType

  is_a val NumberType = val `is_a` IntegerType || val `is_a` FloatType
  is_a val ytype = ytype == typeOf val
