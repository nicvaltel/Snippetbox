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

type FunctionName = String

logWithFunctionName :: (KatipContext m) => Severity -> Namespace -> LogStr -> m ()
logWithFunctionName severity ns msg = do
  -- Add the namespace (Module and Function name)
  katipAddNamespace ns $ do
    logFM severity msg
