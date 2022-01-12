module Homepage.Server (
  main,
) where

import Homepage.Server.Config (Config (..), withConfig)
import qualified Homepage.Server.Http as Http

main :: IO ()
main = withConfig $ \Config{port, staticPath, getRenderedHtml} ->
  Http.listenOn staticPath getRenderedHtml port
