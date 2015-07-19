module Main where
import System.Environment
import Parser
import Value (showValue)
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

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "exit") (readPrompt "ymir: ") . evalAndPrint

main :: IO ()
main =
  do
    args <- getArgs
    case length args of
      0 -> runRepl
      1 -> runOne $ args !! 0
      _ -> putStrLn "Program takes only 0 or 1 argument"
