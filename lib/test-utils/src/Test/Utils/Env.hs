-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
module Test.Utils.Env
  ( withEnv,
    withAddedEnv,
    clearEnv,
  )
where

import Control.Monad.IO.Unlift
  ( MonadIO,
    MonadUnliftIO (..),
  )
import UnliftIO.Environment
  ( getEnvironment,
    setEnv,
    unsetEnv,
  )
import UnliftIO.Exception
  ( bracket,
  )
import Prelude

-- | Runs an IO action with exactly the given environment variables.
-- After the action finishes, the original environment variables are restored.
--
-- NOTE: The process environment is a global variable -- this function is not
-- threadsafe.
withEnv :: MonadUnliftIO m => [(String, String)] -> m a -> m a
withEnv = withEnv' clearEnvs

-- | Runs an IO action with the given environment variables set, in addition to
-- the current environment variables. After the action finishes, the
-- environment variables are restored back to the original state.
--
-- NOTE: The process environment is a global variable -- this function is not
-- threadsafe.
withAddedEnv :: MonadUnliftIO m => [(String, String)] -> m a -> m a
withAddedEnv = withEnv' (const $ pure ())

-- | Runs an action with the given environment variables set.
withEnv' ::
  MonadUnliftIO m =>
  -- | Prepare environment function - given the current environment.
  ([(String, String)] -> m ()) ->
  -- | Environment variables to set.
  [(String, String)] ->
  -- | Action to run with environment variables set.
  m a ->
  m a
withEnv' prepare env = bracket getEnvironment resetEnvironment . run
  where
    resetEnvironment pre = clearEnv >> setEnvs pre
    setEnvs = mapM_ (uncurry setEnv)
    run action = \pre -> prepare pre >> setEnvs env >> action

-- | Unsets all environment variables for this process.
clearEnv :: MonadIO m => m ()
clearEnv = getEnvironment >>= clearEnvs

clearEnvs :: MonadIO m => [(String, a)] -> m ()
clearEnvs = mapM_ (unsetEnv . fst)
