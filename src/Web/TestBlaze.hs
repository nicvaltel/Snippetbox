module Web.TestBlaze where


import ClassyPrelude
import Web.Scotty
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- Data structure that holds the template data 
data PageData = PageData
  { pgTitle   :: Text
  , pgId      :: Int
  , pgContent :: Text
  }

-- Create a template using Blaze
htmlTemplate :: PageData -> H.Html
htmlTemplate pageData = docTypeHtml $ do
  H.head $ H.title "Simple Page"
  H.body $ do
    H.div ! A.class_ "metadata" $ do
      H.strong $ H.toHtml $ pgTitle pageData
      H.span $ "#" >> H.toHtml (pgId pageData)
    H.pre $ H.code $ H.toHtml $ pgContent pageData

-- Main Scotty App
mainTest :: IO ()
mainTest = scotty 3000 $ do
  get "/" $ do
    -- Dummy data for rendering
    let page = PageData "My Title" 123 "This is the content of the page."
    Web.Scotty.html $ renderHtml $ htmlTemplate page