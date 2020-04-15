module Main where
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Class
import Environment
import Error
import Eval
import Interpreter
import Parser
import Primitives
import System.Console.Haskeline
import System.Directory (getCurrentDirectory)
import System.Environment
import System.FilePath
import System.IO hiding (try)
import Value

-- |Parse Ymir code and evaluate it.
evalString :: Env -> String -> Interp Env String
evalString env exp = fmap show $ lifting >>= eval
  where lifting = Interp $ \e -> liftThrows $ (\a -> (e, a)) <$> readExpr exp

-- |Evaluate an expression and then print its result.
evalAndPrint :: Env -> String -> Interp Env ()
evalAndPrint env expr = Interp $ \e -> do
  (e2, str) <- runInterp (evalString env expr) e
  nil <- lift $! putStrLn str
  return (e2, nil)

-- |Load each file given as paremeter and run them one after the other.
runOne :: [String] -> IO ()
runOne args =
  do
    dir <- getCurrentDirectory
    let env = primitiveBindings ++ binds dir
    runIOThrows $ show <$> runInterp res env -- >>= hPutStrLn stderr
    return ()
  where
    binds dir = [("args", List list), ("@@file", String ""), ("@@dir", String dir)]
    list = map String $ drop 1 args
    res = eval (List [Atom "require-relative", String (head args)])

-- |Run the interactive interpreter.
runRepl :: IO ()
runRepl = do
  dir <- getCurrentDirectory
  let bindings = primitiveBindings ++ [atatfile, ("@@dir", String dir)]
  runInputT defaultSettings (loop $! bindings)
  where
    atatfile = ("@@file", String "")

    loop :: Env -> InputT IO ()
    loop env = do
      minput <- getInputLine "ymir % "
      case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input ->
          let interp = evalAndPrint env input in
          do
            either <- lift $ runExceptT $! runInterp interp env
            case either of
              Left err -> do
                lift $! print err
                loop env
              Right (env', _) -> loop $! env'

main :: IO ()
main =
  do
    args <- getArgs
    if null args
      then runRepl
      else runOne args
