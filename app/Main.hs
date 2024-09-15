module Main (main) where

import ClassyPrelude
import qualified Lib
import Options.Applicative
import qualified Data.Text as T
import Test.Tasty.Options (safeRead)
import qualified PostgreSQL.Common as PG
import qualified Configuration.Dotenv as Dotenv
import Data.Either.Combinators (maybeToRight)
import Text.Read (read)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BSC8

newtype Params = Params 
  { portText :: Maybe Text
  }

mkParams :: Parser Params
mkParams =
  Params <$> optional (strOption $
                long "address" <> short 'a' <>
                help "Port adress")

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "Snippetbox server powered by Haskell")



readDBConfig :: String -> IO (Either String PG.Config)
readDBConfig file = do
  env <- Dotenv.parseFile file
  let result :: Either String PG.Config = do
        dbHost <- maybeToRight "No Hostname defined" (lookup "POSTGRES_HOST" env)
        dbPort :: Int <- maybeToRight "No port number defined" (read <$> lookup "POSTGRES_PORT" env)
        dbName <- maybeToRight "No database name defined" (lookup "POSTGRES_DB" env)
        dbUser <- maybeToRight "No username defined" (lookup "POSTGRES_USER" env)
        dbPassword <- maybeToRight "No password defined" (lookup "POSTGRES_PASSWORD" env)
        configStripeCount <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_STRIPE_COUNT" env)
        dbMaxOpenConnPerStripe <- maybeToRight "No max open connections per stripe defined" (read <$> lookup "POSTGRES_MAX_OPEN_CONN_PER_STRIPE" env)
        dbIdleConnTimeout <- maybeToRight "No stripe count defined" (read <$> lookup "POSTGRES_IDLE_CONN_TIMEOUT" env)
        let configUrl :: String  = printf "postgresql://%s:%s@%s:%d/%s" dbUser dbPassword dbHost dbPort dbName 
        pure PG.Config {PG.configUrl = BSC8.pack configUrl, PG.configStripeCount = configStripeCount, PG.configMaxOpenConnPerStripe = dbMaxOpenConnPerStripe, PG.configIdleConnTimeout = dbIdleConnTimeout}
  pure result



-- Run like: cabal run Snippetbox-exe -- -a 4000
main :: IO ()
main = do
  Params{portText} <- cmdLineParser
  let port = case safeRead . T.unpack <$> portText :: Maybe (Maybe Int) of
            Just (Just p) | p > 0 && p < 65536 -> p
            _ -> 3000

  pgCfg <- either error id <$> readDBConfig "db/database.env"
  -- Lib.withAppState port pgCfg (\port_ le_ appState_ -> runKatip $ Web.Main.runWebServer port)
  putStrLn $ "starting server on :" <> tshow port
  Lib.runApp port pgCfg

