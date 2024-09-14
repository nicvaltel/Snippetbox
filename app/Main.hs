module Main (main) where

import ClassyPrelude
import qualified Web.Main
import Options.Applicative
import qualified Data.Text as T
import Test.Tasty.Options (safeRead)
import Logger
import qualified PostgreSQL.Snippetbox as PG
import Control.Concurrent(getNumCapabilities)

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


-- Run like: cabal run Snippetbox-exe -- -a 4000
main :: IO ()
main = do
  nKernelTreads <- getNumCapabilities
  Params{portText} <- cmdLineParser
  let port = case safeRead . T.unpack <$> portText :: Maybe (Maybe Int) of
            Just (Just p) | p > 0 && p < 65536 -> p
            _ -> 3000

  PG.withState pgCfg{PG.configMaxOpenConnPerStripe = max 2 (nKernelTreads - 2) } (\pool -> runKatip $ Web.Main.runWebServer port)
  -- runKatip $ Web.Main.runWebServer port


pgCfg = PG.Config
  { PG.configUrl = "postgresql://***:***@localhost:6666/snippetbox_db"
  , PG.configStripeCount = 2
  , PG.configMaxOpenConnPerStripe = 5
  , PG.configIdleConnTimeout = 10
}