{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module PostgreSQL.Common where


import ClassyPrelude
import Database.PostgreSQL.Simple
    ( close, connectPostgreSQL, withTransaction, Connection )
import Database.PostgreSQL.Simple.Migration
    ( runMigrations,
      MigrationCommand(MigrationDirectory, MigrationInitialization),
      MigrationResult(MigrationError) )
import Data.Pool
    ( withResource,
      defaultPoolConfig,
      destroyAllResources,
      newPool,
      Pool )
import Control.Monad.Catch (MonadThrow)
import Data.Has (Has (getter))

type PoolConnection = Pool Connection

type PG r m = (Has (Pool Connection) r, MonadReader r m, MonadIO m, MonadThrow m)

data Config = Config
  { configUrl :: ByteString
  , configStripeCount :: Int
  , configMaxOpenConnPerStripe :: Int
  , configIdleConnTimeout :: Double -- in seconds NominalDiffTime
  }

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getter
  liftIO . withResource pool $ \conn -> action conn


withState :: Config -> (Pool Connection -> IO a) -> IO a
withState cfg action =
  withPool cfg $ \pool -> do
    migrate pool
    action pool
    

withPool :: Config -> (Pool Connection -> IO a) -> IO a
withPool cfg action = do
  bracket initPool cleanPool action
  where
    initPool = 
      newPool $ defaultPoolConfig
        openConn
        closeConn
        (configIdleConnTimeout cfg)
        (configMaxOpenConnPerStripe cfg * configStripeCount cfg)

    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close


migrate :: Pool Connection -> IO ()
migrate pool =
  withResource pool $ \conn -> do
    res <- withTransaction conn (runMigrations False conn cmds)
    case res of
      MigrationError err -> throwString err
      _ -> pure ()
  where
    cmds =
      [ MigrationInitialization
      , MigrationDirectory "src/PostgreSQL/Migrations"
      ]