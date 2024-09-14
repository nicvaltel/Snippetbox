{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module PostgreSQL.Snippetbox where


import ClassyPrelude
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import Data.Pool
import Control.Monad.Catch (MonadThrow)
import Data.Has (Has (getter))

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
  putStrLn $ "configMaxOpenConnPerStripe cfg * configStripeCount cfg = " <> (tshow $ configMaxOpenConnPerStripe cfg * configStripeCount cfg)
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