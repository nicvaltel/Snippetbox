module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
import Utils.Utils (logger)


runWebServer :: IO ()
runWebServer = do
  logger "starting server on :3000"
  scottyT 3000 id routes




routes :: (MonadIO m, MonadUnliftIO m) => ScottyT m ()
routes = do
  get "/" home



home :: MonadIO m => ActionT m ()
home = do
  text "Hello from Snippetbox"
