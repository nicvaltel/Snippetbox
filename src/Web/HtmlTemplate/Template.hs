module Web.HtmlTemplate.Template where

import ClassyPrelude
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Model (Snippet (..))
import qualified Data.Text as T
import Data.Time (Year)


baseTemplate :: Year -> Html -> Html -> Html -> Html
baseTemplate year titleContent navContent mainContent = H.docTypeHtml $ do
  H.html ! A.lang "en" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.title $ do
        titleContent
        " - Snippetbox"
      H.link ! A.rel "stylesheet" ! A.href "/static/css/main.css"
      H.link ! A.rel "shortcut icon" ! A.href "/static/img/favicon.ico" ! A.type_ "image/x-icon"
      H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css?family=Ubuntu+Mono:400,700"

    H.body $ do
      H.header $ do
        H.h1 $ H.a ! A.href "/" $ "Snippetbox"
      navContent
      H.main mainContent
      H.footer $ do
        "Powered by "
        H.a ! A.href "https://hackage.haskell.org/package/scotty/" $ "Scotty"
        " in " <> H.text (tshow year)
      H.script ! A.src "/static/js/main.js" ! A.type_ "text/javascript" $ mempty


homeTemplate :: Year -> [Snippet] -> Html
homeTemplate year snippets = baseTemplate year "Home" navTemplate content
  where 
    content = do
        H.h2 "Latest Snippets"
        case snippets of
          [] ->
            H.p "There's nothing to see here yet!"
          _ -> do
            H.table $ do
              H.tr $ do
                H.th "Title"
                H.th "Created"
                H.th "ID"
              mapM_ renderSnippetRow snippets
    
    renderSnippetRow :: Snippet -> Html
    renderSnippetRow Snippet{snippetId, snippetTitle, snippetCreated} =
      H.tr $ do
        H.td $ H.a ! A.href (H.toValue $ "/snippet/view/" <> show snippetId) $ H.toHtml snippetTitle
        H.td $ H.toHtml $ formatUTCTime snippetCreated
        H.td $ "#" >> H.toHtml (show snippetId)
            

navTemplate :: Html
navTemplate = H.nav $ do
  H.a ! A.href "/" $ "Home"


veiwTemplate :: Year -> Snippet -> Html
veiwTemplate year Snippet{snippetId, snippetTitle, snippetContent, snippetCreated, snippetExpires} = 
  baseTemplate year "Snippet" navTemplate content
  where
    content :: Html
    content =
      H.div ! A.class_ "snippet" $ do
        H.div ! A.class_ "metadata" $ do
          H.strong $ H.toHtml snippetTitle
          H.span $ "#" >> H.toHtml (show snippetId)
        H.pre $ H.code $ H.toHtml (replaceNewlines snippetContent)
        H.div ! A.class_ "metadata" $ do
          H.time $ "Created: " >> H.toHtml (formatUTCTime snippetCreated)
          H.time $ "Expires: " >> H.toHtml (formatUTCTime snippetExpires)


formatUTCTime :: UTCTime -> String
formatUTCTime = formatTime defaultTimeLocale "%Y-%-m-%-d"


replaceNewlines :: Text -> Text
replaceNewlines = T.replace "\\n" "\n"


renderWithNewlines :: Text -> Html
renderWithNewlines text = mapM_ renderPart (T.splitOn "\n" text)
  where
    renderPart t = H.toHtml t >> H.br


-- Example usage
pageExample :: Html
pageExample = baseTemplate 2024 (H.toHtml ("My Title" :: Text)) navTemplate (H.toHtml ("This is the main content" :: Text))
