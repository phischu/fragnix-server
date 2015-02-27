{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice (
    readSlice,
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment))

import Lucid (
    renderText,Html,
    html_)

import Data.Text.Lazy.IO (writeFile)

import System.Environment (getArgs)

import Prelude hiding (writeFile)

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Number of slices: " ++ show (length args))
    exampleSlice <- readSlice ("slices/" ++ show exampleSliceID)
    writeFile "site/hello.html" (renderText (sliceSite exampleSlice))

exampleSliceID :: SliceID
exampleSliceID = 109962473027768259

sliceSite :: Slice -> Html ()
sliceSite (Slice sliceID language fragment usages) =
    html_ "Hallo World"
