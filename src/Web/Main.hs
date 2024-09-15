module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
    ( defaultOptions,
      ScottyT,
      captureParam,
      get,
      post,
      middleware,
      scottyAppT )
import Web.Handlers ( home, snippetView, snippetCreate, snippetCreatePost )
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Katip ( ls, logTM, Severity(InfoS), KatipContext ) 
import Logger ( runKatip )
import Model (SnippetsRepo)
import Network.Wai (Response, Application)
import qualified Network.Wai.Handler.Warp as Warp


type Port = Int
  
-- runWebServer :: (KatipContext m) => Port -> m ()
-- runWebServer port = do
--   $(logTM) InfoS $ ls ("starting server on :" <> tshow port)
--   scottyT port runKatip routes


mainWeb :: 
  (MonadUnliftIO m, KatipContext m, SnippetsRepo m) =>
  (m Response -> IO Response) -> IO Application
mainWeb runner = do
  -- cacheContainer <- initCaching PublicStaticCaching
  -- scottyAppT defaultOptions runner $ routes cacheContainer
  scottyAppT defaultOptions runner routes

mainRunWebServer :: (MonadUnliftIO m, KatipContext m, SnippetsRepo m) =>
  Int -> (m Response -> IO Response) -> IO ()
mainRunWebServer port runner = do
  web <- mainWeb runner
  runKatip $ $(logTM) InfoS $ ls ("starting server on :" <> tshow port)
  Warp.run port web



routes :: (MonadUnliftIO m, KatipContext m, SnippetsRepo m) => ScottyT m ()
routes = do
  -- Serve static files from the "/ui/" directory
  middleware $ staticPolicy (addBase "ui/")

  get "/" home

  get "/snippet/view/:idx" $ do
    idx <- captureParam "idx"
    snippetView idx

  get "/snippet/create" snippetCreate

  post "/snippet/create" snippetCreatePost


