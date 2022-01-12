module Homepage.Server.Http (
  listenOn,
) where

import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import qualified Data.Text as Text
import Network.Mime (defaultMimeLookup)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (doesFileExist)
import System.FilePath (joinPath)

responseHtml :: ByteString -> Wai.Response
responseHtml html =
  let headers = [("Content-Type", "text/html; charset=UTF-8")]
      status = toEnum 200
   in Wai.responseLBS status headers html

responseNotFound :: Wai.Response
responseNotFound =
  Wai.responseLBS (toEnum 404) [] mempty

responseFile :: FilePath -> Wai.Response
responseFile filePath =
  let mimeType = defaultMimeLookup (Text.pack filePath)
      headers = [("Content-Type", mimeType)]
   in Wai.responseFile (toEnum 200) headers filePath Nothing

application :: FilePath -> IO ByteString -> Wai.Application
application staticPath getRenderedHtml request send =
  case Wai.pathInfo request of
    [] -> do
      renderedHtml <- getRenderedHtml
      send $ responseHtml renderedHtml
    pathParts -> do
      let filePath = joinPath (staticPath : fmap Text.unpack pathParts)
      fileExists <- doesFileExist filePath

      let response
            | fileExists = responseFile filePath
            | otherwise = responseNotFound
       in send response

listenOn :: FilePath -> IO ByteString -> Warp.Port -> IO ()
listenOn staticPath getRenderedHtml port =
  let message = putStrLn $ "Running on http://localhost:" <> show port
      settings =
        Warp.defaultSettings
          & Warp.setBeforeMainLoop message
          & Warp.setPort port
   in Warp.runSettings settings (application staticPath getRenderedHtml)
