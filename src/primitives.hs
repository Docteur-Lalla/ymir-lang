module Primitives where
import Control.Monad.Except
import Environment
import Error
import Parser
import Parser
import System.Directory
import System.FilePath
import System.IO
import Value

primitives :: [(String, [YmirValue] -> ThrowsError YmirValue)]
primitives =
  [
    ("+", numBinop (+)),
    ("-", numBinop (-)),
    ("*", numBinop (*)),
    ("/", numBinop (/)),
    ("%", modNum),
    ("string?", stringType),
    ("symbol?", symbolType),
    ("number?", numberType),
    ("integer?", integerType),
    ("float?", floatType),
    ("bool?", boolType),
    ("char?", charType),
    ("list?", listType),
    ("dotted-list?", dottedListType),
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
    ("cons", cons),
    ("eqv?", eqv)
  ]

primitiveBindings :: Env
primitiveBindings = map makeFunc primitives
  where
    makeFunc (var, func) = (var, Primitive func)

numBinop :: (Number -> Number -> Number) -> [YmirValue] -> ThrowsError YmirValue
numBinop op singleVal@[_] = throwError (NumArgs 2 singleVal)
numBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: YmirValue -> ThrowsError Number
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum (String s) =
  let parsed = reads s in
  if null parsed
    then throwError (TypeMismatch "number" (String s))
    else return $ fst $ head parsed
unpackNum notNum = throwError (TypeMismatch "number" notNum)

modNum :: [YmirValue] -> ThrowsError YmirValue
modNum [Number (Integer a), Number (Integer b)] = return $ Number (Integer $ a `mod` b)
modNum (values:xs) = throwError (TypeMismatch "integer" values)

stringType :: [YmirValue] -> ThrowsError YmirValue
stringType [String _] = return (Bool True)
stringType _ = return (Bool False)

symbolType :: [YmirValue] -> ThrowsError YmirValue
symbolType [Atom _] = return (Bool True)
symbolType _ = return (Bool False)

numberType :: [YmirValue] -> ThrowsError YmirValue
numberType [Number _] = return (Bool True)
numberType _ = return (Bool False)

integerType :: [YmirValue] -> ThrowsError YmirValue
integerType [Number (Integer _)] = return (Bool True)
integerType _ = return (Bool False)

floatType :: [YmirValue] -> ThrowsError YmirValue
floatType [Number (Float _)] = return (Bool True)
floatType _ = return (Bool False)

boolType :: [YmirValue] -> ThrowsError YmirValue
boolType [Bool _] = return (Bool True)
boolType _ = return (Bool False)

charType :: [YmirValue] -> ThrowsError YmirValue
charType [Char _] = return (Bool True)
charType _ = return (Bool False)

listType :: [YmirValue] -> ThrowsError YmirValue
listType [List _] = return (Bool True)
listType _ = return (Bool False)

dottedListType :: [YmirValue] -> ThrowsError YmirValue
dottedListType [DottedList _ _] = return (Bool True)
dottedListType _ = return (Bool False)

showType :: [YmirValue] -> ThrowsError YmirValue
showType [List _] = return $ String "list"
showType [DottedList _ _] = return $ String "dotted-list"
showType [String _] = return $ String "string"
showType [Atom _] = return $ String "symbol"
showType [Char _] = return $ String "char"
showType [Number (Integer _)] = return $ String "integer"
showType [Number (Float _)] = return $ String "float"
showType [Bool _] = return $ String "bool"
showType [Pointer _] = return $ String "pointer"
showType _ = return $ String "unknown"

boolBinop :: (YmirValue -> ThrowsError a) -> (a -> a -> Bool) -> [YmirValue] -> ThrowsError YmirValue
boolBinop unpacker op args
  | length args /= 2 = throwError (NumArgs 2 args)
  | otherwise =
    do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool (left `op` right)

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool

unpackBool :: YmirValue -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError (TypeMismatch "bool" notBool)

car :: [YmirValue] -> ThrowsError YmirValue
car [String (x:xs)] = return (Char x)
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError (TypeMismatch "pair" badArg)
car badArgList = throwError (NumArgs 1 badArgList)

cdr :: [YmirValue] -> ThrowsError YmirValue
cdr [String (x:xs)] = return (String xs)
cdr [List (x:xs)] = return (List xs)
cdr [DottedList [xs] x] = return x
cdr [DottedList (_:xs) x] = return (DottedList xs x)
cdr [badArg] = throwError (TypeMismatch "pair" badArg)
cdr badArgList = throwError (NumArgs 1 badArgList)

cons :: [YmirValue] -> ThrowsError YmirValue
cons [Char c, String s] = return (String (c:s))
cons [x1, List []] = return (List [x1])
cons [x, List xs] = return (List $ x:xs)
cons [x, DottedList xs xlast] = return (DottedList (x:xs) xlast)
cons [x1, x2] = return (DottedList [x1] x2)
cons badArgList = throwError (NumArgs 2 badArgList)

eqv :: [YmirValue] -> ThrowsError YmirValue
eqv [Bool arg1, Bool arg2] = return $ Bool (arg1 == arg2)
eqv [Number arg1, Number arg2] = return $ Bool (arg1 == arg2)
eqv [String arg1, String arg2] = return $ Bool (arg1 == arg2)
eqv [Atom arg1, Atom arg2] = return $ Bool (arg1 == arg2)
eqv [Char arg1, Char arg2] = return $ Bool (arg1 == arg2)
eqv [DottedList xs x, DottedList ys y] = eqv [List left, List right]
  where
    left = xs ++ [x]
    right = ys ++ [y]
eqv [List arg1, List arg2] = return $ Bool (len && values)
  where
    len = length arg1 == length arg2
    values = and $ zipWith (curry eqvPair) arg1 arg2

    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError (NumArgs 2 badArgList)

require :: String -> String -> IOThrowsError [YmirValue]
require dir filename = liftIO (readFile filename') >>= (liftThrows . readExprList filename)
  where
    filename' = case dir of
      "." -> filename
      _ -> normalise (dir ++ "/" ++ filename)
