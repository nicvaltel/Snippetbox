{-# LANGUAGE ScopedTypeVariables #-}
module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
import Utils.Utils (logger)
import Test.Tasty.Options (safeRead)
import qualified Data.Text as T
import Network.HTTP.Types.Status ( notFound404 )


runWebServer :: IO ()
runWebServer = do
  logger "starting server on :3000"
  scottyT 3000 id routes




routes :: (MonadIO m, MonadUnliftIO m) => ScottyT m ()
routes = do
  get "/" home
  get "/snippet/view/:idx" $ do
    idx <- captureParam "idx"
    snippetView idx
  get "/snippet/create" snippetCreate



home :: MonadIO m => ActionT m ()
home = do
  text "Hello from Snippetbox"



snippetView :: MonadIO m => Text -> ActionT m ()
snippetView idx = do
  case safeRead (T.unpack idx) :: Maybe Int of
    Nothing -> do
      status notFound404 
      text "404 page not found"
    Just idn -> text $ "Display a specific snippet with ID " <> fromStrict idx


snippetCreate :: MonadIO m => ActionT m ()
snippetCreate = do
  text "Display a form for creating a new snippet..."

