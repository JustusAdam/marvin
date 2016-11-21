{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import           ClassyPrelude
import           Data.Aeson            hiding (object)
import           System.Directory
import           System.FilePath
import           Text.Mustache
import           Text.Mustache.Compile
import           Options.Applicative
import qualified Data.Configurator as C
import Marvin.Run (defaultConfigName, lookupFromAppConfig)


data Opts = Opts
    { adapter         :: Maybe String
    , sourceName      :: FilePath
    , sourceLocation  :: FilePath
    , targetFile      :: FilePath
    , externalScripts :: FilePath
    , configLocation  :: Maybe FilePath
    }


slackRtmData = ("Marvin.Adapter.Slack", "SlackRTMAdapter")


adapters :: [(String, (String, String))]
adapters =
    [("slack-rtm", slackRtmData)]


tpl :: Template
tpl = $(embedTemplate ["preprocessor"] "Main.mustache")


main :: IO ()
main = do
    Opts{..} <- execParser infoParser

    (adapterImport, adapterType) <- maybe 
        (do
            (cfg, _) <- autoReload autoConfig [Optional configLocation]
            fromConf <- lookupFromAppConfig cfg "adapter"
            return $ fromMaybe slackRtmData $ fromConf >>= flip lookup adapters
        ) 
        return 
        adapter

    let dir = takeDirectory sourceName
    paths <- filter (not . ((||) <$> isPrefixOf "_" <*> isPrefixOf ".")) <$> getDirectoryContents dir
    files <- filterM (doesFileExist . (dir </>)) paths
    externals <- do
        exists <- doesFileExist externalScripts
        if exists
            then do
                f <- readFile externalScripts
                either error return $ eitherDecode f
            else return mempty
    let hsFiles = map dropExtensions $ filter (/= takeFileName sourceName) $ filter ((`elem` [".hs", ".lhs"]) . takeExtension) files
        scripts = hsFiles ++ externals
        (adapterImport, adapterType) = fromMaybe slackRtmData $ lookup adapter adapters
        processed = substitute tpl (object [ "scripts" ~> intercalate ", " (map (++ ".script") scripts)
                                            , "imports" ~> scripts
                                            , "adapter-import" ~> adapterImport
                                            , "adapter-type" ~> adapterType
                                            ])
    writeFile targetFile processed
  where
    infoParser = info
        (helper <*> optsParser)
        (fullDesc ++ header "marvin-pp ~ the marvin preprocessor")
    optsParser = Opts
        <$> optional 
            (strOption
                $  long "adapter"
                ++ short 'a'
                ++ metavar "ID"
                ++ help "adapter to use"
                ++ showDefault
            )
        <*> argument str (metavar "NAME")
        <*> argument str (metavar "PATH")
        <*> argument str (metavar "PATH")
        <*> strOption
            (  long "external-scripts"
            ++ short 's'
            ++ value "external-scripts.json"
            ++ metavar "PATH"
            ++ help "config file of external scripts to load"
            ++ showDefault
            )
        <*> strOption
            (  long "config-location"
            ++ short 'c'
            ++ metavar "PATH"
            ++ help "config to use"
            ++ showDefault
            ++ value defaultConfigName
            )
