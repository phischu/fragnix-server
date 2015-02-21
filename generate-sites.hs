{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (writeFile)

import Lucid (
    renderText,Html,
    html_)
import Data.Text.Lazy.IO (writeFile)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    print (length args)
    writeFile "site/hello.html" (renderText site)

site :: Html ()
site = html_ "Hallo World"
