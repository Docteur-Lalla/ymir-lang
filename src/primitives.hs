module Primitives where
import Value
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

primitives :: [(String, [YmirValue] -> ThrowsError YmirValue)]
primitives =
  [
    ("+", numBinop (+)),
    ("-", numBinop (-)),
    ("*", numBinop (*)),
    ("/", numBinop div),
    ("%", numBinop mod),
    ("quotient", numBinop quot),
    ("rem", numBinop rem),
    ("string?", stringType),
    ("symbol?", symbolType),
    ("number?", numberType),
    ("bool?", boolType),
    ("char?", charType),
    ("type", showType)
  ]

numBinop :: (Integer -> Integer -> Integer) -> [YmirValue] -> ThrowsError YmirValue
numBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: YmirValue -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum (String s) =
  let parsed = reads s
    in if null parsed
      then throwError (TypeMismatch "number" (String s))
      else return $ fst (parsed !! 0)
unpackNum notNum = throwError (TypeMismatch "number" notNum)

stringType :: [YmirValue] -> ThrowsError YmirValue
stringType [(String _)] = return (Bool True)
stringType _ = return (Bool False)

symbolType :: [YmirValue] -> ThrowsError YmirValue
symbolType [(Atom _)] = return (Bool True)
symbolType _ = return (Bool False)

numberType :: [YmirValue] -> ThrowsError YmirValue
numberType [(Number _)] = return (Bool True)
numberType _ = return (Bool False)

boolType :: [YmirValue] -> ThrowsError YmirValue
boolType [(Bool _)] = return (Bool True)
boolType _ = return (Bool False)

charType :: [YmirValue] -> ThrowsError YmirValue
charType [(Char _)] = return (Bool True)
charType _ = return (Bool False)

showType :: [YmirValue] -> ThrowsError YmirValue
showType [(List _)] = return $ String "list"
showType [(String _)] = return $ String "string"
showType [(Char _)] = return $ String "char"
showType [(Number _)] = return $ String "number"
showType [(Bool _)] = return $ String "bool"
showType _ = return $ String "unknown"

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
