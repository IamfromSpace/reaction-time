module Main where

import           AWS.Lambda.Context (HasLambdaContext (..))
import           AWS.Lambda.Runtime (mRuntimeWithContext)
import           Control.Lens       (set)
import           Data.Text          (pack)
import           Lib                (handler)
import           Network.AWS        (Credentials (Discover), Env,
                                     LogLevel (Debug), envLogger, newEnv,
                                     newLogger, runAWS, runResourceT)
import           System.Environment (getEnv)
import           System.IO          (stderr)

-- TODO: This is an awkward interaction that needs some thought.
-- It needs to not only have the AWS Env in the reader, but also the LambdaContext.
-- probably want a helper to promote it.
instance HasLambdaContext Env where
  withContext _ x = x

main :: IO ()
main = do
  tableName <- pack <$> getEnv "TABLE_NAME"
  lgr <- newLogger Debug stderr
  env <- newEnv Discover
  runResourceT $
      runAWS (set envLogger lgr env) $
      mRuntimeWithContext $ handler tableName
