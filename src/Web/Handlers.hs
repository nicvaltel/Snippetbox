module Web.Handlers where


import ClassyPrelude
import Web.Scotty.Trans
import Test.Tasty.Options (safeRead)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status ( notFound404, status201, internalServerError500 )
import Web.HtmlTemplate.Template
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.Directory (doesFileExist)
import Katip ( ls, logTM, Severity(..), KatipContext, showLS, katipAddNamespace, logFM, Namespace )
import Model (SnippetsRepo(..), CommonData (..))
import Text.Blaze.Html (Html)


checkAndRenderHtmlFile :: (KatipContext m) => FilePath -> ActionT m ()
checkAndRenderHtmlFile path = do
  exist <- liftIO $ doesFileExist path
  if exist
    then file path
    else lift $ $(logTM) ErrorS $ ls ("Handlers home error file " <> T.pack path <> " doesn't exist")

checkAndRenderHtmlTemplate :: (KatipContext m, MonadUnliftIO m) => Html -> Namespace -> (SomeException -> Text) -> ActionT m ()
checkAndRenderHtmlTemplate htmlTemplate namespace mkErrMsg = do
  result :: Either SomeException TL.Text <- try (evaluate $ force $ renderHtml htmlTemplate)
  case result of
    Left err -> do
      lift $ katipAddNamespace namespace $ 
        logFM ErrorS (showLS $ mkErrMsg err)
      -- lift $ logWithFunctionName ErrorS "Handlers snippetView" ("renderHtml (viewTemplate snippet) error: " <> showLS (tshow err))
      -- lift $ $(logTM) ErrorS $ ls ("Handlers snippetView: renderHtml (viewTemplate snippet) error: " <> show err)
      status internalServerError500
      text $ "An error occurred: " <> TL.pack (show err)
    Right txt -> do
      html txt

home :: (MonadUnliftIO m, SnippetsRepo m, KatipContext m, CommonData m) => ActionT m ()
home = do
  addHeader "Server" "Haskell Scotty"
  snippets <- lift latestSnippets
  year <- lift getCurrentYear
  -- html $ renderHtml $ homeTemplate year snippets
  checkAndRenderHtmlTemplate
    (homeTemplate year snippets)
    "Handlers home"
    (\err -> "renderHtml (homeTemplate snippets) error: " <> tshow err)


snippetView :: (MonadUnliftIO m, SnippetsRepo m, KatipContext m, CommonData m) => Text -> ActionT m ()
snippetView idx = do
  case safeRead (T.unpack idx) :: Maybe Int of
    Just idn | idn > 0 -> do
      maySnippet <- lift $ getSnippet idn
      case maySnippet of
        Just snippet -> do
          year <- lift getCurrentYear
          -- html $ renderHtml $ veiwTemplate year snippet
          checkAndRenderHtmlTemplate 
            (veiwTemplate year snippet)
            "Handlers snippetView"
            (\err -> "renderHtml (viewTemplate snippet) error: " <> tshow err <> " ; snippetId = " <> tshow idx)
        Nothing -> do
          status notFound404
          text "Not found"
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
