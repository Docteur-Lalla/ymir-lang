module Eval where
import Control.Monad.Except
import Data.Maybe (isNothing, maybeToList)
import Environment
import Function
import Interpreter
import Primitives
import System.FilePath
import Value

eval :: YmirValue -> Interp Env YmirValue
eval val@(String _) = return val
eval val@(Char _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id) = readVar id
eval (DottedList x xs) = do
  x' <- mapM eval x
  xs' <- eval xs
  return $ DottedList x' xs'
eval (List [Atom "define", Atom var, form]) = do
  val <- eval form
  defineNewVar var val
  return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom "define":List (Atom var:params):body)) = do
  env <- environment
  let function = makeNormalFunc env params body
  defineNewVar var function
  return function
eval (List (Atom "define":DottedList (Atom var:params) varargs:body)) = do
  env <- environment
  let function = makeVarargs varargs env params body
  defineNewVar var function
  return function
eval (List (Atom "def-macro":List (Atom var:params):body)) = do
  env <- environment
  let macro = makeNormalMacro env params body
  defineNewVar var macro
  return macro
eval (List (Atom "def-macro":DottedList (Atom var:params) varargs:body)) = do
  env <- environment
  let macro = makeVarargsMacro varargs env params body
  defineNewVar var macro
  return macro
eval (List (Atom "lambda":List params:body)) = do
  env <- environment
  return $ makeNormalFunc env params body
eval (List (Atom "lambda":DottedList params varargs:body)) = do
  env <- environment
  return $ makeVarargs varargs env params body
eval (List (Atom "lambda":varargs@(Atom _):body)) = do
  env <- environment
  return $ makeVarargs varargs env [] body
eval (List [Atom "require-relative", String file]) = requireFile True dir file'
  where
    dir = takeDirectory file
    file' = takeFileName file
eval (List (f:args)) = do
  env <- environment
  func <- eval f
  let applyFunction = apply env eval func
  case func of
    Macro params varargs body -> prepareEval params varargs body args applyFunction
    Closure params varargs body _ -> prepareEval params varargs body args applyFunction

    Primitive _ -> do
      argVals <- mapM eval args
      apply env eval func argVals
    _ -> failWith (NotFunction "Given value is not a function" $ show f)

eval badForm = failWith $ BadSpecialForm "Unrecognized special form" badForm

-- |Evaluation the arguments that should not be quoted before actually calling
-- |the function.
prepareEval :: [Parameter]
            -> Maybe Parameter
            -> [YmirValue]
            -> [YmirValue]
            -> ([YmirValue] -> Interp Env YmirValue)
            -> Interp Env YmirValue
prepareEval params varargs body args f = do
  let computeArgM = mapM computeArg
  argVals <- computeArgM (zipped params args)
  varArgVals <- computeArgM (bindVarArgs params args varargs)
  if differentLengths args params && isNothing varargs
    then do
      dropped <- mapM eval (remainingArguments params args)
      failWith $ NumArgs (num params) (argVals ++ dropped)
    else f (argVals ++ varArgVals)
  where
    num = toInteger . length
    zipped params args = zip params (take (length params) args)

-- |Evaluate the argument if it should be evaluated.
computeArg :: (Parameter, YmirValue) -> Interp Env YmirValue
computeArg ((_, True), value) = eval (List [Atom "quote", value])
computeArg ((_, False), value) = eval value

varArgs :: [Parameter] -> [YmirValue] -> Maybe (String, Bool) -> [YmirValue]
varArgs params args = concat . maybeToList . fmap (const $ remainingArguments params args)

-- |Bind the variadic arguments to the variadic parameter name.
bindVarArgs :: [Parameter] -> [YmirValue] -> Maybe Parameter -> [(Parameter, YmirValue)]
bindVarArgs params args =
  let bind var = map ((,) var) (remainingArguments params args) in
  concat . maybeToList . fmap bind

-- |Load an Ymir source file and execute its contents.
requireFile :: Bool -> String -> String -> Interp Env YmirValue
requireFile relative dir file = do
  file' <- getCurrentFile
  dir' <- getCurrentDir
  setCurrentFile (String file)
  setRelativeCurrentDir relative dir'
  cdir <- getCurrentDir
  res <- (Interp $ \e -> (,) e <$> fileStatements cdir) >>= ($!) (fmap last . mapM eval)
  setCurrentFile file'
  setCurrentDir dir'
  return res
  where
    dirstr (String s) = s
    fileStatements cdir = require (dirstr cdir) file

    -- If the command is require-relative, the current directory variable is
    -- computed from the current directory, otherwise the given directory is
    -- used as an absolute path.
    setRelativeCurrentDir :: Bool -> YmirValue -> Interp Env ()
    setRelativeCurrentDir relative dir'
      | relative = setNormalisedCurrentDir $ dirstr dir' ++ "/" ++ dir
      | otherwise = setNormalisedCurrentDir dir
      where setNormalisedCurrentDir = setCurrentDir . String . normalise

getCurrentFile :: Interp Env YmirValue
getCurrentFile = readVar "@@file"

getCurrentDir :: Interp Env YmirValue
getCurrentDir = readVar "@@dir"

setCurrentFile :: YmirValue -> Interp Env ()
setCurrentFile = writeVar "@@file"

setCurrentDir :: YmirValue -> Interp Env ()
setCurrentDir = writeVar "@@dir"
