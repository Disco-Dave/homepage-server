module Homepage.Server.Config (
  Config (..),
  withConfig,
) where

import qualified Control.Concurrent.STM as STM
import Control.Monad (void)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.IO as TextIO
import qualified Data.Yaml as Yaml
import Homepage.Server.Config.CliArgs (CliArgs (..), getCliArgs)
import qualified Homepage.Server.Page as Page
import Homepage.Server.Page.Data (PageData, StaticAssets (..))
import qualified Network.Wai.Handler.Warp as Warp
import qualified Paths_homepage_server as Paths
import qualified System.FSNotify as FSNotify
import qualified System.FilePath as FilePath

data Config = Config
  { port :: Warp.Port
  , staticPath :: FilePath
  , getRenderedHtml :: IO ByteString
  }

withConfig :: (Config -> IO a) -> IO a
withConfig useConfig = do
  CliArgs{..} <- getCliArgs

  baseScript <-
    Paths.getDataFileName "data/index.js"
      >>= TextIO.readFile
  baseStyles <-
    Paths.getDataFileName "data/index.css"
      >>= TextIO.readFile
  searchIcon <-
    Paths.getDataFileName "data/search-solid.svg"
      >>= TextIO.readFile

  let staticAssets = StaticAssets{..}
      renderHtml = do
        pageData <- Yaml.decodeFileThrow @_ @PageData cliFile
        pure $ Page.renderHtml staticAssets pageData

  initialHtml <- renderHtml
  renderedHtmlVar <- STM.newTVarIO initialHtml

  let watchPath = FilePath.takeDirectory cliFile
      isConfigFile =
        (cliFile ==) . \case
          FSNotify.Added path _ _ -> path
          FSNotify.Modified path _ _ -> path
          FSNotify.Unknown path _ _ -> path
          FSNotify.Removed path _ _ -> path

  let config =
        Config
          { port = cliPort
          , staticPath = cliStatic
          , getRenderedHtml = STM.readTVarIO renderedHtmlVar
          }

  FSNotify.withManager $ \manager -> do
    void . FSNotify.watchDir manager watchPath isConfigFile $ \_ -> do
      putStrLn "Detected change, rendering html"
      newHtml <- renderHtml
      STM.atomically $ STM.writeTVar renderedHtmlVar newHtml

    useConfig config
