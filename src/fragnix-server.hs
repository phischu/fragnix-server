{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (
  scotty, get, html, liftAndCatchIO)

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
  get "/" (
    html (renderHtml indexHtml))
  get (fromString environmentsRoute) (
    html (renderHtml environmentsHtml))
  get (fromString (environmentRoute "scotty")) (do
    moduleNames <- liftAndCatchIO (listDirectory ("data/" ++ environmentRoute "scotty"))
    html (renderHtml (environmentHtml "scotty" moduleNames))))


indexHtml :: Html
indexHtml = docTypeHtml (body (do
  "Hello and welcome to fragnix online!"
  li ((a ! href "/environments") "environments")))

environmentsHtml :: Html
environmentsHtml = docTypeHtml (body (do
  "To get you started, we offer the following environments:"
  ul ((a ! href "/environments/scotty") "scotty")))

environmentsRoute :: String
environmentsRoute = "/environments"

environmentRoute :: String -> String
environmentRoute environmentName = environmentsRoute ++ "/" ++ environmentName

environmentHtml :: String -> [String] -> Html
environmentHtml environmentName moduleNames = docTypeHtml (body (do
  "The " >> toHtml environmentName >> " environment consists of the following modules:"
  ul (forM_ moduleNames (\moduleName -> do
    let environmentModuleRouteAttribute = fromString (
          environmentModuleRoute environmentName moduleName)
    li ((a ! (href environmentModuleRouteAttribute)) (toHtml moduleName))))))

environmentModuleRoute :: String -> String -> String
environmentModuleRoute environmentName moduleName = do
  environmentRoute environmentName ++ "/" ++ moduleName



