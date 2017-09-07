{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module FragnixServer where


import Fragnix.Slice (
  Slice(Slice), SliceID, Use(Use), Reference(OtherSlice),
  readSlice, writeSlice)
import Fragnix.Environment (
  loadEnvironment)
import Fragnix.SliceCompiler (
  usedSliceIDs, sliceInstanceIDs)
import Fragnix.DeclarationSlices (
  moduleReference)
import Servant (
  Get, Post, ReqBody, JSON, (:>), Capture, (:<|>)((:<|>)),
  serve, Server, Application)
import Language.Haskell.Exts (
  ModuleName(ModuleName))
import Language.Haskell.Names (
  Environment, Symbol, symbolModule)

import qualified Data.Map as Map (
  toList)
import Control.Monad (
  forM, unless, forM_)
import Control.Monad.IO.Class (
  liftIO)
import Control.Monad.Trans.State.Strict (
  StateT,execStateT,get,put)
import Data.Proxy (
  Proxy(Proxy))


type API =
  EnvironmentAPI :<|>
  EnvironmentSlicesAPI :<|>
  SliceTransitiveAPI :<|>
  SlicePublishAPI


application :: Application
application = serve (Proxy :: Proxy API) (
  serveEnvironment :<|>
  serveEnvironmentSlices :<|>
  serveSliceTransitive :<|>
  serveSlicePublish)


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


type EnvironmentSlicesAPI =
  "environment" :> Capture "environmentName" String :> "slices" :> Get '[JSON] [Slice]

serveEnvironmentSlices :: Server EnvironmentSlicesAPI
serveEnvironmentSlices environmentName = do
  let environmentPath = "data/environments/" ++ environmentName
  environment <- liftIO (loadEnvironment environmentPath)
  let referencedSliceIDs = do
        (_, symbols) <- Map.toList environment
        symbol <- symbols
        case moduleReference (symbolModule symbol) of
          OtherSlice sliceID -> return sliceID
          _ -> []
  sliceIDs <- liftIO (execStateT (forM referencedSliceIDs loadSliceIDsStateful) [])
  slices <- liftIO (forM sliceIDs (\sliceID -> do
    readSlice ("data/slices/" ++ show sliceID)))
  return slices


type SliceTransitiveAPI =
  "slices" :> Capture "sliceID" SliceID :> "transitive" :> Get '[JSON] [Slice]

serveSliceTransitive :: Server SliceTransitiveAPI
serveSliceTransitive sliceID = do
  sliceIDs <- liftIO (execStateT (loadSliceIDsStateful sliceID) [])
  slices <- liftIO (forM sliceIDs (\sliceID -> do
    readSlice ("data/slices/" ++ show sliceID)))
  return slices


loadSliceIDsStateful :: SliceID -> StateT [SliceID] IO ()
loadSliceIDsStateful sliceID = do
    seenSliceIDs <- get
    unless (elem sliceID seenSliceIDs) (do
        put (sliceID : seenSliceIDs)
        slice <- liftIO (readSlice ("data/slices/" ++ show sliceID))
        let recursiveSliceIDs = usedSliceIDs slice
            recursiveInstanceSliceIDs = sliceInstanceIDs slice
        forM_ recursiveSliceIDs loadSliceIDsStateful
        forM_ recursiveInstanceSliceIDs loadSliceIDsStateful)


type SlicePublishAPI =
  "slices" :> "publish" :> ReqBody '[JSON] [Slice] :> Post '[JSON] ()

serveSlicePublish :: Server SlicePublishAPI
serveSlicePublish slices = do
  liftIO (forM_ slices (\slice@(Slice sliceID _ _ _ _) ->
    writeSlice ("data/slices/" ++ show sliceID) slice))

