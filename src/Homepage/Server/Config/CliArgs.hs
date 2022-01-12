module Homepage.Server.Config.CliArgs (
  CliArgs (..),
  getCliArgs,
) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as Opt
import qualified System.Directory as Directory
import System.FilePath ((</>))

data CliArgs = CliArgs
  { cliPort :: Warp.Port
  , cliFile :: FilePath
  , cliStatic :: FilePath
  }

getCliDefaults :: IO CliArgs
getCliDefaults = do
  let appDirectory = "homepage-server"

  configFile <- do
    xdgConfig <- Directory.getXdgDirectory Directory.XdgConfig appDirectory
    pure $ xdgConfig </> "config.yaml"

  staticPath <- Directory.getXdgDirectory Directory.XdgData appDirectory

  pure
    CliArgs
      { cliPort = 50000
      , cliFile = configFile
      , cliStatic = staticPath
      }

cliArgs :: CliArgs -> Opt.Parser CliArgs
cliArgs defaultArgs =
  let port =
        Opt.option Opt.auto . mconcat $
          [ Opt.long "port"
          , Opt.short 'p'
          , Opt.metavar "<port>"
          , Opt.help "Port the http server runs on"
          , Opt.value (cliPort defaultArgs)
          , Opt.showDefault
          ]
      file =
        Opt.strOption . mconcat $
          [ Opt.long "file"
          , Opt.short 'p'
          , Opt.metavar "<filepath>"
          , Opt.help "Path to the configuration yaml file"
          , Opt.value (cliFile defaultArgs)
          , Opt.showDefault
          ]
      static =
        Opt.strOption . mconcat $
          [ Opt.long "static"
          , Opt.short 's'
          , Opt.metavar "<filepath>"
          , Opt.help "Path to search for static assets in"
          , Opt.value (cliStatic defaultArgs)
          , Opt.showDefault
          ]
   in CliArgs
        <$> port
        <*> file
        <*> static

getCliArgs :: IO CliArgs
getCliArgs = do
  parser <- fmap cliArgs getCliDefaults

  let infoMods =
        mconcat
          [ Opt.fullDesc
          , Opt.header "homepage-server"
          , Opt.progDesc "Run a local homepage server for use in your web browser"
          ]

  Opt.execParser (Opt.info (Opt.helper <*> parser) infoMods)
