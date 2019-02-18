module Main where

import           AWS.Lambda.ApiGatewayRuntime (mRuntimeWithContext)
import           AWS.Lambda.Context           (HasLambdaContext (..))
import           Control.Lens                 (set)
import           Lib                          (handler)
import           Network.AWS                  (Credentials (Discover), Env,
                                               LogLevel (Debug), envLogger,
                                               newEnv, newLogger, runAWS,
                                               runResourceT)
import           System.IO                    (stderr)

-- TODO: This is an awkward interaction that needs some thought.
-- It needs to not only have the AWS Env in the reader, but also the LambdaContext.
-- probably want a helper to promote it.
instance HasLambdaContext Env where
  withContext _ x = x

main :: IO ()
main = do
  lgr <- newLogger Debug stderr
  env <- newEnv Discover
  runResourceT $
      runAWS (set envLogger lgr env) $
      mRuntimeWithContext handler
