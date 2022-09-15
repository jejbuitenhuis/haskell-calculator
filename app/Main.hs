module Main (main) where

import Types
import Parser
import qualified Control.Monad
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

test :: (Fractional n) => AST n
test = Expr (Number $ Just 1) Add (Expr (Number $ Just 2) Sub (Number $ Just 3))

loop :: IO ()
loop = do
    putStr "> "
    input <- getLine
    Control.Monad.when (input /= "quit" && input /= "exit") $ do
        print (parse input)
        loop

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "=#= Calculator =#="
    putStrLn "To exit, type `exit` or `quit`"
    loop
