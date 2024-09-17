{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Web.Main where


import ClassyPrelude
import Web.Scotty.Trans
    ( defaultOptions,
      ScottyT,
      captureParam,
      get,
      post,
      middleware,
      scottyAppT )
import Web.Handlers ( home, snippetView, snippetCreate, snippetCreatePost )
import Network.Wai.Middleware.Static (addBase, staticPolicy, initCaching, CachingStrategy (PublicStaticCaching), CacheContainer, staticPolicy')
import Katip
    ( KatipContext,
      Severity(InfoS, ErrorS),
      ls,
      sl,
      katipAddContext,
      logFM,
      logTM, showLS ) 
import Logger ( runKatip )
import Model (SnippetsRepo, CommonData)
import Network.Wai (Response, Application, Middleware, Request (remoteHost, requestMethod, rawPathInfo, httpVersion), ResponseReceived, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as GZ
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.HTTP.Types (status500)

type Port = Int


mainWeb :: 
  (MonadUnliftIO m, KatipContext m, SnippetsRepo m, CommonData m) =>
  (m Response -> IO Response) -> IO Application
mainWeb runner = do
  cacheContainer <- initCaching PublicStaticCaching
  scottyAppT defaultOptions runner $ routes cacheContainer
  -- scottyAppT defaultOptions runner routes

mainRunWebServer :: (MonadUnliftIO m, KatipContext m, SnippetsRepo m, CommonData m) =>
  Int -> (m Response -> IO Response) -> IO ()
mainRunWebServer port runner = do
  web <- mainWeb runner
  runKatip $ $(logTM) InfoS $ ls ("starting server on :" <> tshow port)
  Warp.run port web

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived 
-- type Middleware = Application -> Application
appLogMiddleware :: Middleware
appLogMiddleware nextAppAction =
  \req sendResponse -> do
    print req
    runKatip $ 
      katipAddContext (sl "remoteSocket" (show $ remoteHost req)) $
      katipAddContext (sl "proto" (show $ httpVersion req)) $
      katipAddContext (sl "method" (show $ requestMethod req)) $
      katipAddContext (sl "uri" (show $ rawPathInfo req)) $
      logFM InfoS "received request"
    nextAppAction req sendResponse

appRecoverErrorMiddleware :: Middleware
appRecoverErrorMiddleware nextAppAction req sendResponse = do
    nextAppAction req sendResponse `catch` handler
  where
    handler :: SomeException -> IO ResponseReceived
    handler err = do
      runKatip $
        logFM ErrorS ("Caught exception: " ++ showLS err)
      sendResponse $ responseLBS
        status500
        [("Content-Type", "text/plain"), ("Connection", "close")]
        "Internal Server Error"

appHeadersMiddleware :: Middleware
appHeadersMiddleware = addHeaders 
  [ ("Server", "Haskell Scotty")
  , ("Content-Security-Policy", "default-src 'self'; style-src 'self' fonts.googleapis.com; font-src fonts.gstatic.com")
  , ("Referrer-Policy", "origin-when-cross-origin")
  , ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "deny")
  , ("X-XSS-Protection", "0")
  ] 

routes :: (MonadUnliftIO m, KatipContext m, SnippetsRepo m, CommonData m) => CacheContainer -> ScottyT m ()
routes cachingStrategy = do
  
  middleware $ GZ.gzip $ GZ.def {GZ.gzipFiles = GZ.GzipCompress}

  -- Serve static files from the "/ui/" directory
  middleware $ staticPolicy' cachingStrategy (addBase "ui/")
  -- middleware $ staticPolicy (addBase "ui/")

  middleware appRecoverErrorMiddleware -- redundant error handling (scotty do it by itself)

  middleware appLogMiddleware
  
  middleware appHeadersMiddleware

  get "/" home

  get "/snippet/view/:idx" $ do
    idx <- captureParam "idx"
    snippetView idx

  get "/snippet/create" snippetCreate

  post "/snippet/create" snippetCreatePost


