{-# LANGUAGE OverloadedStrings #-}
module Main where

import Fragnix.Slice (
    readSlice,
    Slice(Slice),SliceID,
    Language(Language),Fragment(Fragment),
    Use(Use),UsedName(..),Name(..),Reference(..))

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (
    Html,toHtml,AttributeValue,toValue,
    docTypeHtml,body,div,pre,h3,a,(!))
import Text.Blaze.Html5.Attributes (href)

import Data.Text (Text,unlines,pack)
import Data.Text.Lazy.IO (writeFile)

import System.Directory (getDirectoryContents,createDirectoryIfMissing)
import System.FilePath ((</>),(<.>))

import Control.Monad (forM_)
import Data.Monoid ((<>))
import Prelude hiding (writeFile,div,unlines)

main :: IO ()
main = do
    sliceNames <- getDirectoryContents "slices" >>=
        return . filter (not . (=='.') . head)
    putStrLn ("Number of slices: " ++ show (length sliceNames))
    createDirectoryIfMissing True "site"
    forM_ sliceNames (\sliceName -> do
        slice <- readSlice ("slices" </> sliceName)
        writeFile ("site" </> sliceName <.> "html") (renderHtml (sliceHtml slice)))

sliceHtml :: Slice -> Html
sliceHtml (Slice sliceID language fragment uses) = docTypeHtml (do
    body (do
        fragmentHtml fragment
        usesHtml uses))

fragmentHtml :: Fragment -> Html
fragmentHtml (Fragment declarations) = do
    h3 "Fragment"
    pre (toHtml (unlines declarations))

usesHtml :: [Use] -> Html
usesHtml uses = do
    h3 "Uses"
    forM_ uses (\(Use maybeQualification usedName reference) -> do
        let usednamehtml = pre (usedNameHtml usedName)
        case reference of
            OtherSlice otherSliceID -> do
                (a ! href (sliceURL otherSliceID)) usednamehtml
            _ -> do
                usednamehtml)

usedNameHtml :: UsedName -> Html
usedNameHtml (ValueName name) = nameHtml name
usedNameHtml (TypeName name) = nameHtml name
usedNameHtml (ConstructorName _ name) = nameHtml name
usedNameHtml Instance = "<some instance>"

nameHtml :: Name -> Html
nameHtml (Identifier identifier) = toHtml identifier
nameHtml (Operator operator) = toHtml operator

sliceURL :: SliceID -> AttributeValue
sliceURL sliceID = toValue (show sliceID ++ ".html")

languageHtml :: Language -> Html
languageHtml (Language extensions) = do
    h3 "Language extensions"
    forM_ extensions (\extension -> do
        pre (toHtml extension))
