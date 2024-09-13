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
        H.body $ do
            H.header $ do
                H.h1 $ H.a ! A.href "/" $ "Snippetbox"
            navContent
            H.main mainContent
            H.footer $ do
                "Powered by "
                H.a ! A.href "https://hackage.haskell.org/package/scotty/" $ "Scotty"

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
