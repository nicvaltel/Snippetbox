module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
import Web.Handlers ( home, snippetView, snippetCreate, snippetCreatePost )
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Katip 
import Logger

type Port = Int

  
runWebServer :: (KatipContext m) => Port -> m ()
runWebServer port = do
  $(logTM) InfoS $ ls ("starting server on :" <> tshow port)
  scottyT port runKatip routes


routes :: (MonadUnliftIO m, KatipContext m) => ScottyT m ()
routes = do
  -- Serve static files from the "/ui/" directory
  middleware $ staticPolicy (addBase "ui/")

  get "/" home

  get "/snippet/view/:idx" $ do
    idx <- captureParam "idx"
    snippetView idx

  get "/snippet/create" snippetCreate

  post "/snippet/create" snippetCreatePost


