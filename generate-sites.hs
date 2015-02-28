{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice (
    readSlice,
    Slice(Slice),SliceID,Language(Language),Fragment(Fragment))

import Lucid (
    renderText,Html,
    html_)

import Data.Text.Lazy.IO (writeFile)

import System.Directory (getDirectoryContents)
import System.FilePath ((</>),(<.>))

import Control.Monad (forM_)
import Prelude hiding (writeFile)

main :: IO ()
main = do
    sliceNames <- getDirectoryContents "slices" >>=
        return . filter (not . (=='.') . head)
    putStrLn ("Number of slices: " ++ show (length sliceNames))
    forM_ sliceNames (\sliceName -> do
        slice <- readSlice ("slices" </> sliceName)
        writeFile ("site" </> sliceName <.> "html") (renderText (sliceSite slice)))

sliceSite :: Slice -> Html ()
sliceSite (Slice sliceID language fragment usages) =
    html_ "Hallo World"
