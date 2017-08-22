{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (
  scotty, get,
  ActionM, html, liftAndCatchIO)


import Text.Blaze.Html.Renderer.Text (
  renderHtml)
import Text.Blaze.Html5 (
  Html, toHtml, docTypeHtml, (!),
  body, ul, li, a)
import Text.Blaze.Html5.Attributes (
  href)

import System.Directory (
  listDirectory)
import Data.Text (
  Text)
import Control.Monad (
  forM_)
import Data.String (
  fromString)


main :: IO ()
main = scotty 3000 (do
  get "/" serveIndex
  get "/environments" serveEnvironments
  get "/environments/scotty" serveEnvironment)


serveIndex :: ActionM ()
serveIndex = do
  html (renderHtml indexHtml)

indexHtml :: Html
indexHtml = docTypeHtml (body (do
  "Hello and welcome to fragnix online!"
  li ((a ! href "/environments") "environments")))


serveEnvironments :: ActionM ()
serveEnvironments = do
  html (renderHtml environmentsHtml)

environmentsHtml :: Html
environmentsHtml = docTypeHtml (body (do
  "To get you started, we offer the following environments:"
  ul ((a ! href "/environments/scotty") "scotty")))


serveEnvironment :: ActionM ()
serveEnvironment = do
  moduleNames <- liftAndCatchIO (listDirectory ("data/environments/scotty"))
  html (renderHtml (environmentHtml "scotty" moduleNames))

environmentHtml :: String -> [String] -> Html
environmentHtml environmentName moduleNames = docTypeHtml (body (do
  "The " >> toHtml environmentName >> " environment consists of the following modules:"
  ul (forM_ moduleNames (\moduleName -> do
    li (toHtml moduleName)))))


