module Logger where

import ClassyPrelude
import Katip

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