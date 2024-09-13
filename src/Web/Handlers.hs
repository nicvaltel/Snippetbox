module Web.Handlers where


import ClassyPrelude
import Web.Scotty.Trans
import Test.Tasty.Options (safeRead)
import qualified Data.Text as T
import Network.HTTP.Types.Status ( notFound404, status201 )


home :: MonadIO m => ActionT m ()
home = do
  addHeader "Server" "Haskell Scotty"
  file "UI/html/pages/home.html"


snippetView :: MonadIO m => Text -> ActionT m ()
snippetView idx = do
  case safeRead (T.unpack idx) :: Maybe Int of
    Just idn | idn > 0 -> do
      text $ "Display a specific snippet with ID " <> fromStrict idx
    _ -> do
      status notFound404 
      text "Not found"


snippetCreate :: MonadIO m => ActionT m ()
snippetCreate = do
  text "Display a form for creating a new snippet..."


snippetCreatePost :: MonadIO m => ActionT m ()
snippetCreatePost = do
  status status201
  text "Save a new snippet..."
