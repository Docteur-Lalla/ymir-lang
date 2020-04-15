module Main where
import Control.Monad.Trans.Class
import Control.Monad
import Eval (eval)
import Error
import Parser
import Primitives
import System.Console.Haskeline
import System.Directory (getCurrentDirectory)
import System.Environment
import System.FilePath
import System.IO hiding (try)
import Value
import Variable

-- |Parse Ymir code and evaluate it.
evalString :: Env -> String -> IO String
evalString env exp = runIOThrows $ fmap show $ lifting >>= eval env
  where lifting = liftThrows $ readExpr exp

-- |Evaluate an expression and then print its result.
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

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
runRepl = do
  dir <- getCurrentDirectory
  bindings <- primitiveBindings >>= flip bindVars [atatfile, ("@@dir", String dir)]
  let eval = evalAndPrint bindings
  runInputT defaultSettings (loop eval)
  where
    atatfile = ("@@file", String "")

    loop :: (String -> IO ()) -> InputT IO ()
    loop eval = do
      minput <- getInputLine "ymir % "
      case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
          lift $ eval input
          loop eval

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then runRepl
      else runOne args
