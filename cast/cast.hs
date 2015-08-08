module Cast where
import Value
import FFI
import Control.Monad.Error
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Array
import System.IO.Unsafe

type HaskellYmirFunction = [YmirValue] -> ThrowsError YmirValue

string :: [YmirValue] -> ThrowsError YmirValue
string [value] = (return . String . showValue) value
string values = throwError $ NumArgs 1 values

symbol :: [YmirValue] -> ThrowsError YmirValue
symbol [String s] = (return . Atom) s
symbol [Char c] = (return . Atom . show) c
symbol [x] = throwError $ TypeMismatch "string or char" x
symbol values = throwError $ NumArgs 1 values

integer :: [YmirValue] -> ThrowsError YmirValue
integer [String s] = return $ Number (Integer (read s))
-- integer [Number (Float f)] = return $ Number (Integer f)
integer [val@(Number (Integer _))] = return val
integer [x] = throwError $ TypeMismatch "string or number" x
integer values = throwError $ NumArgs 1 values

float :: [YmirValue] -> ThrowsError YmirValue
float [String s] = return $ Number (Float (read s))
-- float [Number (Integer i)] = return $ Number (Float (fromIntegral i))
float [val@(Number (Float _))] = return val
float [x] = throwError $ TypeMismatch "string or number" x
float values = throwError $ NumArgs 1 values

-- C bindings
c_bind_function :: HaskellYmirFunction -> Ptr ValuePtr -> Int -> ReturnValuePtr
c_bind_function f ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ f list

c_string = c_bind_function string
c_symbol = c_bind_function symbol
c_integer = c_bind_function integer
c_float = c_bind_function float

foreign export ccall c_string :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_symbol :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_integer :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_float :: Ptr ValuePtr -> Int -> ReturnValuePtr
