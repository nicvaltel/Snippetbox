module Web.Handlers where


import ClassyPrelude
import Web.Scotty.Trans
import Test.Tasty.Options (safeRead)
import qualified Data.Text as T
import Network.HTTP.Types.Status ( notFound404, status201 )
import Web.HtmlTemplate.Template (homeTemplate)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Directory (doesFileExist)
import Katip


checkAndRenderHtmlFile :: (KatipContext m) => FilePath -> ActionT m ()
checkAndRenderHtmlFile path = do
  exist <- liftIO $ doesFileExist path
  if exist
    then file path
    else lift $ $(logTM) ErrorS $ ls ("Handlers home error file " <> T.pack path <> " doesn't exist")


handler :: IOException -> IO T.Text
handler e = do
  putStrLn "File not found or cannot be opened."
  return ""


home :: (MonadIO m, KatipContext m) => ActionT m ()
home = do
  addHeader "Server" "Haskell Scotty"
  html $ renderHtml homeTemplate
  checkAndRenderHtmlFile "ui/html/pages/home.html"


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
