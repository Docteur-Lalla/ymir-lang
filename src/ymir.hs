module Main where
import System.Environment
import Parser
import Value (showValue)
import Primitives
import Eval (eval)
import Control.Monad
import Error

main :: IO ()
main =
  do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
