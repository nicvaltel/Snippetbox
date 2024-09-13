module Web.HtmlTemplate.Template where

import ClassyPrelude
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5.Attributes as A


baseTemplate :: Html -> Html -> Html -> Html
baseTemplate titleContent navContent mainContent = H.docTypeHtml $ do
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
            H.script ! A.src "/static/js/main.js" ! A.type_ "text/javascript" $ mempty


homeTemplate :: Html
homeTemplate = 
    let title = "Home"
        content = do
            H.h2 "Latest Snippets"
            H.p "There's nothing to see here yet!"
     in baseTemplate title navTemplate content


navTemplate :: Html
navTemplate = H.nav $ do
    H.a ! A.href "/" $ "Home"


-- Example usage
pageExample :: Html
pageExample = baseTemplate (H.toHtml ("My Title" :: Text)) navTemplate (H.toHtml ("This is the main content" :: Text))
