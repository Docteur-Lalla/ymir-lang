module Main where
import System.Environment
import Parser
import Value
import Primitives
import Eval (eval)
import Control.Monad
import Error
import System.IO hiding (try)
import Variable
import System.FilePath
import System.Directory (getCurrentDirectory)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Print a prompt asking the user for input and then read a line of code.
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- |Parse Ymir code and evaluate it.
evalString :: Env -> String -> IO String
evalString env exp = runIOThrows $ fmap show $ lifting >>= eval env
  where lifting = liftThrows $ readExpr exp

-- |Evaluate an expression and then print its result.
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do
    result <- prompt
    if pred result
      then return ()
      else action result >> until_ pred prompt action

-- |Load each file given as paremeter and run them one after the other.
runOne :: [String] -> IO ()
runOne args =
  do
    dir <- getCurrentDirectory
    env <- primitiveBindings >>= flip bindVars (binds dir)
    runIOThrows $ show <$> res env -- >>= hPutStrLn stderr
    return ()
  where
    binds dir = [("args", List list), ("@@file", String ""), ("@@dir", String dir)]
    list = map String $ drop 1 args
    res env = eval env (List [Atom "require-relative", String (head args)])

-- |Run the interactive interpreter.
runRepl :: IO ()
runRepl =
  do
    dir <- getCurrentDirectory
    bindings <- primitiveBindings >>= flip bindVars [atatfile, ("@@dir", String dir)]
    (until_ (== "exit") (readPrompt "ymir: ") . evalAndPrint) bindings
  where
    atatfile = ("@@file", String "")

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then runRepl
      else runOne args
