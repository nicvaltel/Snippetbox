module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
import Utils.Utils (logger)
import Web.Handlers ( home, snippetView, snippetCreate, snippetCreatePost )


runWebServer :: IO ()
runWebServer = do
  logger "starting server on :3000"
  scottyT 3000 id routes


routes :: (MonadUnliftIO m) => ScottyT m ()
routes = do
  get "/" home

  get "/snippet/view/:idx" $ do
    idx <- captureParam "idx"
    snippetView idx

  get "/snippet/create" snippetCreate

  post "/snippet/create" snippetCreatePost


