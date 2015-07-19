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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env exp = runIOThrows $ liftM show $ lifting >>= eval env
  
  where lifting = liftThrows $ readExpr exp

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do
    result <- prompt
    if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args =
  do
    env <- primitiveBindings >>= flip bindVars [("args", List list)]
    (runIOThrows $ liftM show $ res env) >>= hPutStrLn stderr

  where
    list = map String $ drop 1 args
    res env = eval env (List [Atom "require", String (args !! 0)])

runRepl :: IO ()
runRepl =
  do
    bindings <- primitiveBindings
    (until_ (== "exit") (readPrompt "ymir: ") . evalAndPrint) bindings

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then runRepl
      else runOne $ args
