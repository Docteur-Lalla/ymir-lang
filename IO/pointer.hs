module Pointer where
import Value
import FFI
import Data.IORef
import System.IO.Unsafe
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Error

makeRef value = unsafePerformIO $
  do
    ref <- newIORef value
    stable <- newStablePtr ref
    return (castStablePtrToPtr stable)

pointer :: [YmirValue] -> ThrowsError YmirValue
pointer [value] = return $ (Pointer . makeRef) value

pointerGet :: [YmirValue] -> ThrowsError YmirValue
pointerGet [Pointer ptr] = return $ unsafePerformIO $
  do
    ref <- deRefStablePtr (castPtrToStablePtr ptr)
    readIORef ref

pointerSet :: [YmirValue] -> ThrowsError YmirValue
pointerSet [Pointer ptr, val] = return $ unsafePerformIO $
  do
    ref <- deRefStablePtr (castPtrToStablePtr ptr)
    writeIORef ref val
    return val
pointerSet (val:xs) = throwError (TypeMismatch "pointer" val)

c_pointer ary 1 = unsafePerformIO $
  do
    ptrlist <- peekArray 1 ary
    list <- mapM deRefStablePtr ptrlist
    let val = pointer list
    newStablePtr val
c_pointer ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ throwError (NumArgs 1 list)

c_pointer_get ary 1 = unsafePerformIO $
  do
    ptrlist <- peekArray 1 ary
    list <- mapM deRefStablePtr ptrlist
    let val = pointerGet list
    newStablePtr val
c_pointer_get ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ throwError (NumArgs 1 list)

c_pointer_set ary 2 = unsafePerformIO $
  do
    ptrlist <- peekArray 2 ary
    list <- mapM deRefStablePtr ptrlist
    let val = pointerSet list
    newStablePtr val
c_pointer_set ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ throwError (NumArgs 2 list)
    
foreign export ccall c_pointer :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_pointer_get :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_pointer_set :: Ptr ValuePtr -> Int -> ReturnValuePtr
