module Main where
import System.Environment
import Parser
import Primitives
import Eval (showValue, eval)
import Control.Monad

main :: IO ()
main =
  do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
