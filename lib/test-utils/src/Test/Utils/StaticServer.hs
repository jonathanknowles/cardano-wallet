-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides a simple static files web server to be used as a fixture in tests
-- which need a HTTP server.
module Test.Utils.StaticServer
  ( withStaticServer,
  )
where

import Network.Wai.Application.Static
  ( defaultFileServerSettings,
    staticApp,
  )
import Network.Wai.Handler.Warp
  ( withApplication,
  )
import Prelude

-- | Run a localhost HTTP file server on any free port, while executing the
-- given action.
withStaticServer ::
  -- | Web server root directory
  FilePath ->
  -- | Action, taking base URL
  (String -> IO a) ->
  IO a
withStaticServer root action =
  withApplication (pure app) $ \port -> action (baseUrl port)
  where
    app = staticApp $ defaultFileServerSettings root
    baseUrl port = "http://localhost:" <> show port <> "/"
