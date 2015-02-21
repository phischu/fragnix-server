module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print (length args)
    writeFile "site/hello.html" "Hello World"
