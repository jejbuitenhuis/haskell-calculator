module Main (main) where

import Parser
import qualified Control.Monad
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

loop :: IO ()
loop = do
    putStr "> "
    input <- getLine
    Control.Monad.when (input /= "quit" && input /= "exit") $ do
        print $ execute input
        loop

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "=#= Calculator =#="
    putStrLn "To exit, type `exit` or `quit`"
    loop
