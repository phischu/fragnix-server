{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice (readSlice,Slice)

import Lucid (
    renderText,Html,
    html_)

import Data.Text.Lazy.IO (writeFile)

import System.Environment (getArgs)

import Prelude hiding (writeFile)

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Number of slices: " ++ length args)
    writeFile "site/hello.html" (renderText site)

sliceSite :: Slice -> Html ()
sliceSite (Slice sliceID language fragment usages) =
    html_ "Hallo World"
