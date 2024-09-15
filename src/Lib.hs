{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import ClassyPrelude
import Model ( SnippetsRepo(..) )
import qualified PostgreSQL.Common as PG
import qualified PostgreSQL.Snippetbox as PG

import Katip
    ( runKatipContextT, Katip, LogEnv, KatipContext, KatipContextT )
import Control.Monad (MonadFail)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Logger (withKatip)
import qualified Web.Main
import Web.Main (Port)


type AppState = PG.PoolConnection -- Try change Mem.MemState to TVar Mem.State

newtype App a = App { unApp :: ReaderT AppState (KatipContextT IO) a  } 
  deriving (Functor, Applicative, Monad, MonadReader AppState, MonadIO, MonadUnliftIO, MonadFail, MonadThrow, MonadCatch, KatipContext, Katip)


instance SnippetsRepo App where
  insertSnippet = PG.insertSnippet
  getSnippet = PG.getSnippet
  latestSnippets = PG.latestSnippets


runAppState :: LogEnv -> AppState -> App a -> IO a
runAppState le state =
  runKatipContextT le () mempty 
  . flip runReaderT state 
  . unApp


withAppState :: Port -> PG.Config -> (Port -> LogEnv -> AppState -> IO a) -> IO a
withAppState port pgCfg action = do
  withKatip $ \le -> do
    PG.withState pgCfg $ \pgState -> do
      let appState = pgState
      action port le appState


runApp ::  Port -> PG.Config -> IO ()
runApp port pgCfg = do
  withAppState port pgCfg $ \port_ le appState -> do
    let runner = runAppState le appState
    Web.Main.mainRunWebServer port_ runner