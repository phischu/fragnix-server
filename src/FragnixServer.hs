{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module FragnixServer where


import Fragnix.Environment (
  loadEnvironment)
import Servant (
  Get, JSON, (:>), Capture,
  serve, Server, Application)
import Language.Haskell.Exts (
  ModuleName(ModuleName))
import Language.Haskell.Names (
  Environment, Symbol)

import qualified Data.Map as Map (
  toList)
import Control.Monad.IO.Class (
  liftIO)
import Data.Proxy (
  Proxy(Proxy))


application :: Application
application = serve (Proxy :: Proxy EnvironmentAPI) serveEnvironment

type EnvironmentAPI =
  "environment" :> Capture "environmentName" String :> Get '[JSON] [(String, [Symbol])]

serveEnvironment :: Server EnvironmentAPI
serveEnvironment environmentName = do
  let environmentPath = "data/environments/" ++ environmentName
  environment <- liftIO (loadEnvironment environmentPath)
  return (deconstructEnvironment environment)

deconstructEnvironment :: Environment -> [(String, [Symbol])]
deconstructEnvironment =
  map (\(ModuleName () moduleName, symbols) -> (moduleName, symbols)) . Map.toList

