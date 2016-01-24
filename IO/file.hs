module File where
import Value
import FFI

import System.IO
import Data.IORef
import System.IO.Unsafe
import Foreign.StablePtr
import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Except

fileOpen :: [YmirValue] -> ThrowsError YmirValue
fileOpen [String filename] = return $ unsafePerformIO $
  do
    handle <- openFile filename ReadWriteMode
    stable <- newStablePtr handle
    return $ Pointer (castStablePtrToPtr stable)

fileGetline :: [YmirValue] -> ThrowsError YmirValue
fileGetline [Pointer ptr] = return $ unsafePerformIO $
  do
    handle <- deRefStablePtr (castPtrToStablePtr ptr)
    string <- hGetLine handle
    return $ String string

fileClose :: [YmirValue] -> ThrowsError YmirValue
fileClose [Pointer ptr] = return $ unsafePerformIO $
  do
    handle <- deRefStablePtr (castPtrToStablePtr ptr)
    hClose handle
    return (List [])

c_file_open ary 1 = unsafePerformIO $
  do
    ptrlist <- peekArray 1 ary
    list <- mapM deRefStablePtr ptrlist
    let val = fileOpen list
    newStablePtr val
c_file_open ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ throwError (NumArgs 1 list)

c_file_close ary 1 = unsafePerformIO $
  do
    ptrlist <- peekArray 1 ary
    list <- mapM deRefStablePtr ptrlist
    let val = fileClose list
    newStablePtr val
c_file_close ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ throwError (NumArgs 1 list)

c_file_getline ary 1 = unsafePerformIO $
  do
    ptrlist <- peekArray 1 ary
    list <- mapM deRefStablePtr ptrlist
    let val = fileGetline list
    newStablePtr val
c_file_getline ary n = unsafePerformIO $
  do
    ptrlist <- peekArray n ary
    list <- mapM deRefStablePtr ptrlist
    newStablePtr $ throwError (NumArgs 1 list)

foreign export ccall c_file_open :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_file_close :: Ptr ValuePtr -> Int -> ReturnValuePtr
foreign export ccall c_file_getline :: Ptr ValuePtr -> Int -> ReturnValuePtr
