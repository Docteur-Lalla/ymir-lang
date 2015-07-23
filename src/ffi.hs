module FFI where
import Value
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.IO.Unsafe
import System.Posix.DynamicLinker

type ValuePtr = StablePtr YmirValue
type ReturnValuePtr = StablePtr (ThrowsError YmirValue)
type YmirCFunction = Ptr ValuePtr -> Int -> ReturnValuePtr

-- YmirValue creation functions
ymir_newNumber i = unsafePerformIO $ newStablePtr (Number i)
ymir_newChar c = unsafePerformIO $ newStablePtr (Char c)
ymir_newBool b = unsafePerformIO $ newStablePtr (Bool b)
ymir_newString cs = unsafePerformIO $
  do
    s <- peekCString cs
    newStablePtr (String s)
ymir_newList ary len = unsafePerformIO $
  do
    ptrlist <- peekArray len ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr (List list)

-- Function return function
ymir_return :: ValuePtr -> ReturnValuePtr
ymir_return ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    newStablePtr (return val)

-- Type checking functions
ymir_isNumber ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Number _ -> return True
      otherwise -> return False
ymir_isChar ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Char _ -> return True
      otherwise -> return False
ymir_isBool ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Bool _ -> return True
      otherwise -> return False
ymir_isString ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      String _ -> return True
      otherwise -> return False
ymir_isList ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      List _ -> return True
      String _ -> return True -- String are [Char]
      otherwise -> return False

-- Getters
ymir_getNumber ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Number n -> return n
      otherwise -> return 0

ymir_getChar ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Char c -> return c
      otherwise -> return '\0'

ymir_getBool ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Bool b -> return b
      otherwise -> return False

ymir_getString ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      String s -> newCString s
      otherwise -> newCString ""

-- Make a plain Haskell/Ymir primitive from the module and symbol name
makeYmirPrimitive :: String -> String -> [YmirValue] -> ThrowsError YmirValue
makeYmirPrimitive mod sym args = unsafePerformIO $
  do
    dl <- dlopen mod [RTLD_NOW]
    fptr <- dlsym dl sym
    let f = makeYmirCFunction fptr
    argsPtrList <- mapM (\arg -> newStablePtr arg) args
    argsArray <- newArray argsPtrList
    res <- deRefStablePtr $ f argsArray (length args)
    case res of
      Left err -> return ()
      Right v -> return ()
    mapM (\ptr -> freeStablePtr ptr) argsPtrList
    return res

-- Foreign Function Interface
foreign export ccall ymir_newNumber :: Int -> ValuePtr
foreign export ccall ymir_newChar :: Char -> ValuePtr
foreign export ccall ymir_newBool :: Bool -> ValuePtr
foreign export ccall ymir_newString :: CString -> ValuePtr
foreign export ccall ymir_newList :: Ptr ValuePtr -> Int -> ValuePtr

foreign export ccall ymir_return :: ValuePtr -> ReturnValuePtr

foreign export ccall ymir_isNumber :: ValuePtr -> Bool
foreign export ccall ymir_isChar :: ValuePtr -> Bool
foreign export ccall ymir_isBool :: ValuePtr -> Bool
foreign export ccall ymir_isString :: ValuePtr -> Bool
foreign export ccall ymir_isList :: ValuePtr -> Bool

foreign export ccall ymir_getNumber :: ValuePtr -> Int
foreign export ccall ymir_getChar :: ValuePtr -> Char
foreign export ccall ymir_getBool :: ValuePtr -> Bool
foreign export ccall ymir_getString :: ValuePtr -> CString

foreign import ccall "dynamic" makeYmirCFunction :: FunPtr YmirCFunction -> YmirCFunction

loadCModule :: String -> [String] -> IO [[YmirValue] -> ThrowsError YmirValue]
loadCModule modName symbols = mapM (\symbol -> return $ makeYmirPrimitive modName symbol) symbols
