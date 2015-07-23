module Eval where
import Value
import Error
import Control.Monad.Error
import Primitives
import Variable
import System.Environment
import System.Directory
import System.FilePath
import System.IO.Unsafe
import FFI
import Data.IORef

eval :: Env -> YmirValue -> IOThrowsError YmirValue
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "if", pred, true, false]) =
  do
    result <- eval env pred
    case result of
      Bool False -> eval env false
      otherwise -> eval env true
eval env (List [Atom "require", String file]) = requireFile env False (libdir file) file'
  where
    libdir dir =
      case (takeDirectory dir) of
        "." -> "/home/chem/prgm/ymir/lib"
        d -> "/home/chem/prgm/ymir/lib/" ++ d
    file' = takeFileName file
eval env (List [Atom "require-relative", String file]) = requireFile env True dir file'
  where
    dir = takeDirectory file
    file' = takeFileName file
eval env (List [Atom "load", String mod, List syms]) = loadModule env False mod syms
eval env (List [Atom "load-relative", String mod, List syms]) = loadModule env True mod syms
eval env (List (Atom "apply":f:args)) =
  do
    func <- eval env f
    argVals <- mapM (eval env) args
    applyProc (func:argVals)
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List (Atom "eval":list:env')) = eval env list >>= eval env
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "define":List (Atom var:params):body)) = function
  where function = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params) varargs:body)) = function
  where  function = makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda":List params:body)) = makeNormalFunc env params body
eval env (List (Atom "lambda":DottedList params varargs:body)) = lambda
  where lambda = makeVarargs varargs env params body
eval env (List (Atom "lambda":varargs@(Atom _):body)) = lambda
  where lambda = makeVarargs varargs env [] body
eval env (List (f:args)) =
  do
    func <- eval env f
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError (BadSpecialForm "Unrecognized special form" badForm)

makeFunc varargs env params body = return $ Closure pnames varargs body env

  where pnames = map showValue params

makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showValue

applyProc :: [YmirValue] -> IOThrowsError YmirValue
applyProc [f, List args] = apply f args
applyProc (f:args) = apply f args

apply :: YmirValue -> [YmirValue] -> IOThrowsError YmirValue
apply (Primitive f) args = liftThrows $ f args
apply (Closure params varargs body env) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars env $ zip params args) >>=
      bindVarArgs varargs >>= evalBody

  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
      Nothing -> return env

requireFile env relative dir file =
  do
    file' <- getCurrentFile env
    dir' <- getCurrentDir env
    setCurrentFile env (String file)
    if relative
      then setCurrentDir env (String $ normalise (dirstr dir' ++ "/" ++ dir))
      else setCurrentDir env (String $ normalise dir)
    cdir <- getCurrentDir env
    res <- ((require (dirstr cdir) file) >>= ($!) (liftM last . mapM (eval env)))
    setCurrentFile env file'
    setCurrentDir env dir'
    return res
  where
    dirstr (String s) = s        

loadModule env relative mod syms =
  do
    primitives <- if relative
      then liftIO $ loadCModule ("./" ++ mod) symbols
      else liftIO $ loadCModule (dir ++ mod) symbols
    let bindings = zipWith (\a -> \b -> (a, Primitive b)) symbols primitives
    liftIO $
      do
        env' <- bindVars env bindings
        list <- readIORef env'
        writeIORef env list
    getVar env (last symbols)

  where
    dir = "/home/chem/prgm/ymir/lib/"
    symbols = getSymbols syms
    getSymbols [] = []
    getSymbols (Atom x:xs) = x : getSymbols xs
    getSymbols (_:xs) = getSymbols xs

getCurrentFile :: Env -> IOThrowsError YmirValue
getCurrentFile env = getVar env "@@file"

getCurrentDir :: Env -> IOThrowsError YmirValue
getCurrentDir env = getVar env "@@dir"

setCurrentFile :: Env -> YmirValue -> IOThrowsError YmirValue
setCurrentFile env s = setVar env "@@file" s

setCurrentDir :: Env -> YmirValue -> IOThrowsError YmirValue
setCurrentDir env s = setVar env "@@dir" s
