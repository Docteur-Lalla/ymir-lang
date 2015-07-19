module Primitives where
import Value
import Error
import Variable
import Control.Monad.Error

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
    ("type", showType),
    ("=", numBoolBinop(==)),
    ("<", numBoolBinop(<)),
    (">", numBoolBinop(>)),
    ("/=", numBoolBinop(/=)),
    (">=", numBoolBinop(>=)),
    ("<=", numBoolBinop(<=)),
    ("and", boolBoolBinop(&&)),
    ("or", boolBoolBinop(||)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons)
  ]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  
  where makePrimitiveFunc (var, func) = (var, Primitive func)

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

boolBinop :: (YmirValue -> ThrowsError a) -> (a -> a -> Bool) -> [YmirValue] -> ThrowsError YmirValue
boolBinop unpacker op args
  | length args /= 2 = throwError (NumArgs 2 args)
  | otherwise =
    do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool (left `op` right)

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool

unpackBool :: YmirValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "bool" notBool)

car :: [YmirValue] -> ThrowsError YmirValue
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError (TypeMismatch "pair" badArg)
car badArgList = throwError (NumArgs 1 badArgList)

cdr :: [YmirValue] -> ThrowsError YmirValue
cdr [List (x:xs)] = return (List xs)
cdr [DottedList (_:xs) x] = return (DottedList xs x)
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError (TypeMismatch "pair" badArg)
cdr badArgList = throwError (NumArgs 1 badArgList)

cons :: [YmirValue] -> ThrowsError YmirValue
cons [x1, List []] = return (List [x1])
cons [x, List xs] = return (List $ [x] ++ xs)
cons [x, DottedList xs xlast] = return (DottedList ([x] ++ xs) xlast)
cons [x1, x2] = return (DottedList [x1] x2)
cons badArgList = throwError (NumArgs 2 badArgList)
