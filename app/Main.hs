module Main (main) where

import ClassyPrelude
import Web.Main
import Options.Applicative
import qualified Data.Text as T
import Test.Tasty.Options (safeRead)
import Katip


data Params = Params 
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


runKatip :: KatipContextT IO a -> IO a
runKatip app = withKatip $ \le ->
  runKatipContextT le () mempty app

withKatip :: (LogEnv -> IO a) -> IO a
withKatip app =
  bracket createLogEnv closeScribes app
  where
    createLogEnv = do
      logEnv <- initLogEnv "HSnippetbox" "dev"
      stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      registerScribe "stdout" stdoutScribe defaultScribeSettings logEnv

-- Run like: cabal run Snippetbox-exe -- -a 4000
main :: IO ()
main = do
  Params{portText} <- cmdLineParser
  let port = case safeRead . T.unpack <$> portText :: Maybe (Maybe Int) of
            Just (Just p) | p > 0 && p < 65536 -> p
            _ -> 3000

  runKatip $ Web.Main.runWebServer port