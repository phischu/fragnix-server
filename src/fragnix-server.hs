{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (
  scotty, get, html)

import Text.Blaze.Html.Renderer.Text (
  renderHtml)
import Text.Blaze.Html5 (
  Html, docTypeHtml, body)

main :: IO ()
main = scotty 3000 (do
  get "/" (html (renderHtml indexHtml)))


indexHtml :: Html
indexHtml = docTypeHtml (do
  body (do
    "Hello and welcome to fragnix!"))




