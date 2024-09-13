module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
import Utils.Utils (logger)
import Web.Handlers ( home, snippetView, snippetCreate, snippetCreatePost )
import Network.Wai.Middleware.Static (addBase, staticPolicy)

type Port = Int

runWebServer :: Port -> IO ()
runWebServer port = do
  logger $ "starting server on :" <> tshow port
  scottyT port id routes


routes :: (MonadUnliftIO m) => ScottyT m ()
routes = do
  -- Serve static files from the "/ui/" directory
  middleware $ staticPolicy (addBase "ui/")

  get "/" home

  get "/snippet/view/:idx" $ do
    idx <- captureParam "idx"
    snippetView idx

  get "/snippet/create" snippetCreate

  post "/snippet/create" snippetCreatePost


