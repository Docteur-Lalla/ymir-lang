module Extern where
import Value
import Error
import FFI
import Function
import Eval
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Array
import Foreign.C.String
import System.IO.Unsafe
import Control.Monad.Error

-- YmirValue creation functions
ymir_newInteger i = unsafePerformIO $ newStablePtr (Number $ Integer i)
ymir_newFloat f = unsafePerformIO $ newStablePtr (Number $ Float f)
ymir_newChar c = unsafePerformIO $ newStablePtr (Char c)
ymir_newBool b = unsafePerformIO $ newStablePtr (Bool b)
ymir_newString cs = unsafePerformIO $
  do
    s <- peekCString cs
    newStablePtr (String s)
ymir_newSymbol cs = unsafePerformIO $
  do
    s <- peekCString cs
    newStablePtr (Atom s)
ymir_newList ary len = unsafePerformIO $
  do
    ptrlist <- peekArray len ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr (List list)
ymir_newPointer ptr = unsafePerformIO $ newStablePtr (Pointer ptr)

-- Function return function
ymir_return :: ValuePtr -> ReturnValuePtr
ymir_return ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    newStablePtr (return val)

-- Type checking functions
ymir_isInteger ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Number (Integer _) -> return True
      otherwise -> return False
ymir_isFloat ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Number (Float _) -> return True
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
ymir_isSymbol ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Atom _ -> return True
      otherwise -> return False
ymir_isList ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      List _ -> return True
      String _ -> return True -- String are [Char]
      otherwise -> return False
ymir_isPointer ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Pointer _ -> return True
      otherwise -> return False

-- Getters
ymir_getInteger ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Number (Integer n) -> return n
      otherwise -> return 0

ymir_getFloat ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Number (Float f) -> return f
      otherwise -> return 0.0

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
ymir_getSymbol ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Atom s -> newCString s
      otherwise -> newCString ""

ymir_getPointer ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    case val of
      Pointer ptr -> return ptr
      otherwise -> return nullPtr

-- Function call management
ymir_functionCall :: ValuePtr -> Ptr ValuePtr -> Int -> ReturnValuePtr
ymir_functionCall fptr ary size = unsafePerformIO $
  do
    f <- deRefStablePtr fptr
    ptrlist <- peekArray size ary
    list <- mapM deRefStablePtr ptrlist
    res <- runErrorT (applyFunction eval f list)
    newStablePtr res

-- Error handling functions
ymir_throwNumberArguments :: Int -> Ptr ValuePtr -> Int -> ReturnValuePtr
ymir_throwNumberArguments num ary size = unsafePerformIO $
  do
    ptrlist <- peekArray size ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr (throwError (NumArgs (toInteger num) list))

ymir_throwTypeMismatch :: CString -> ValuePtr -> ReturnValuePtr
ymir_throwTypeMismatch str ptr = unsafePerformIO $
  do
    val <- deRefStablePtr ptr
    s <- peekCString str
    newStablePtr (throwError (TypeMismatch s val))

-- Foreign Function Interface
foreign export ccall ymir_newInteger :: Int -> ValuePtr
foreign export ccall ymir_newFloat :: Double -> ValuePtr
foreign export ccall ymir_newChar :: Char -> ValuePtr
foreign export ccall ymir_newBool :: Bool -> ValuePtr
foreign export ccall ymir_newString :: CString -> ValuePtr
foreign export ccall ymir_newSymbol :: CString -> ValuePtr
foreign export ccall ymir_newList :: Ptr ValuePtr -> Int -> ValuePtr
foreign export ccall ymir_newPointer :: Ptr () -> ValuePtr

foreign export ccall ymir_return :: ValuePtr -> ReturnValuePtr

foreign export ccall ymir_isInteger :: ValuePtr -> Bool
foreign export ccall ymir_isFloat :: ValuePtr -> Bool
foreign export ccall ymir_isChar :: ValuePtr -> Bool
foreign export ccall ymir_isBool :: ValuePtr -> Bool
foreign export ccall ymir_isString :: ValuePtr -> Bool
foreign export ccall ymir_isSymbol :: ValuePtr -> Bool
foreign export ccall ymir_isList :: ValuePtr -> Bool
foreign export ccall ymir_isPointer :: ValuePtr -> Bool

foreign export ccall ymir_getInteger :: ValuePtr -> Int
foreign export ccall ymir_getFloat :: ValuePtr -> Double
foreign export ccall ymir_getChar :: ValuePtr -> Char
foreign export ccall ymir_getBool :: ValuePtr -> Bool
foreign export ccall ymir_getString :: ValuePtr -> CString
foreign export ccall ymir_getSymbol :: ValuePtr -> CString
foreign export ccall ymir_getPointer :: ValuePtr -> Ptr ()

foreign export ccall ymir_functionCall :: ValuePtr -> Ptr ValuePtr -> Int -> ReturnValuePtr

foreign export ccall ymir_throwNumberArguments :: Int -> Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall ymir_throwTypeMismatch :: CString -> ValuePtr -> ReturnValuePtr
