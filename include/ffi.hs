module FFI where
import Value
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import System.IO.Unsafe
import System.Posix.DynamicLinker

type ValuePtr = StablePtr YmirValue
type ReturnValuePtr = StablePtr (ThrowsError YmirValue)
type YmirCFunction = Ptr ValuePtr -> Int -> ReturnValuePtr

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

foreign import ccall "dynamic" makeYmirCFunction :: FunPtr YmirCFunction -> YmirCFunction

loadCModule :: String -> [String] -> IO [[YmirValue] -> ThrowsError YmirValue]
loadCModule modName symbols = mapM (\symbol -> return $ makeYmirPrimitive modName symbol) symbols
