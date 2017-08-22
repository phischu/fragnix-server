{-# LANGUAGE OverloadedStrings #-}

import Fragnix.Environment (
  loadEnvironment)
import Web.Scotty (
  scotty, get,
  ActionM, header, param, html, json, liftAndCatchIO)
import Language.Haskell.Exts (
  prettyPrint, ModuleName(ModuleName))
import Language.Haskell.Names (
  Environment, Symbol)

import Text.Blaze.Html.Renderer.Text (
  renderHtml)
import Text.Blaze.Html5 (
  Html, toHtml, docTypeHtml, (!),
  body, ul, li, a)
import Text.Blaze.Html5.Attributes (
  href)
import Data.Aeson (
  ToJSON(toJSON), ToJSONKey, FromJSON(parseJSON), FromJSONKey)

import System.Directory (
  listDirectory)
import Data.Text (
  Text)
import qualified Data.Map as Map (
  toList, keys)
import Control.Monad (
  forM_)
import Data.String (
  fromString)


main :: IO ()
main = scotty 3000 (do
  get "/" serveIndex
  get "/environments" serveEnvironments
  get "/environments/:environmentName" serveEnvironment)


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
  acceptHeader <- header "Accept"
  environmentName <- param "environmentName"
  let environmentPath = "data/environments/" ++ environmentName
  environment <- liftAndCatchIO (loadEnvironment environmentPath)
  case acceptHeader of
    Just "application/json" -> do
      json (deconstructEnvironment environment)
    _ -> do
      let moduleNames = map prettyPrint (Map.keys environment)
      html (renderHtml (environmentHtml environmentName moduleNames))

deconstructEnvironment :: Environment -> [(String, [Symbol])]
deconstructEnvironment =
  map (\(ModuleName () moduleName, symbols) -> (moduleName, symbols)) . Map.toList

environmentHtml :: String -> [String] -> Html
environmentHtml environmentName moduleNames = docTypeHtml (body (do
  "The " >> toHtml environmentName >> " environment consists of the following modules:"
  ul (forM_ moduleNames (\moduleName -> do
    li (toHtml moduleName)))))


