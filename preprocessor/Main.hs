{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
import           Control.Monad         (filterM)
import           Data.Aeson            hiding (object)
import qualified Data.ByteString.Lazy  as B
import qualified Data.Configurator     as C
import           Data.List             (intercalate, isPrefixOf)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Text.IO          as T
import           ExternalScripts
import           Marvin.Run            (defaultConfigName, lookupFromAppConfig)
import           Marvin.Util.Config    (Config(Config))
import           Options.Applicative
import           Prelude
import           System.Directory
import           System.FilePath
import           Text.Mustache
import           Text.Mustache.Compile


data Opts = Opts
    { adapter         :: Maybe String
    , sourceName      :: FilePath
    , sourceLocation  :: FilePath
    , targetFile      :: FilePath
    , externalScripts :: FilePath
    , configLocation  :: Maybe FilePath
    }


slackRtmData :: (String, String)
slackRtmData = ("Marvin.Adapter.Slack.RTM", "(SlackAdapter RTM)")


adapters :: [(String, (String, String))]
adapters =
    [ ("slack-rtm", slackRtmData)
    , ("slack-events", ("Marvin.Adapter.Slack.EventsAPI", "(SlackAdapter EventsAPI)"))
    , ("telegram-poll", ("Marvin.Adapter.Telegram.Poll", "(TelegramAdapter Poll)"))
    , ("telegram-push", ("Marvin.Adapter.Telegram.Push", "(TelegramAdapter Push)"))
    , ("shell", ("Marvin.Adapter.Shell", "ShellAdapter"))
    , ("irc", ("Marvin.Adapter.IRC", "IRCAdapter"))
    ]


tpl :: Template
tpl = $(embedTemplate ["preprocessor"] "Main.mustache")


main :: IO ()
main = do
    Opts{..} <- execParser infoParser

    adapter' <- maybe
        (do
            (cfg, _) <- C.autoReload C.autoConfig $ maybe [] (return . C.Optional) configLocation
            lookupFromAppConfig (Config cfg) "adapter"
        )
        (return . return)
        adapter

    let (adapterImport, adapterType) = fromMaybe slackRtmData $ adapter' >>= flip lookup adapters

    let dir = takeDirectory sourceName
    paths <- filter (not . ((||) <$> isPrefixOf "_" <*> isPrefixOf ".")) <$> getDirectoryContents dir
    files <- filterM (doesFileExist . (dir </>)) paths
    externals <- do
        exists <- doesFileExist externalScripts
        if exists
            then do
                f <- B.readFile externalScripts
                either error return $ eitherDecode f
            else return mempty
    let scriptsFromFiles = map (ModuleOnly . dropExtensions) $ filter (/= takeFileName sourceName) $ filter ((`elem` [".hs", ".lhs"]) . takeExtension) files
        scripts = scriptsFromFiles <> externals
        processed = substitute tpl (object [ "scripts" ~> intercalate ", " (concatMap getScripts scripts)
                                            , "imports" ~> map getModule scripts
                                            , "adapter-import" ~> adapterImport
                                            , "adapter-type" ~> adapterType
                                            ])
    T.writeFile targetFile processed
  where
    infoParser = info
        (helper <*> optsParser)
        (  fullDesc
        <> header "marvin-pp ~ the marvin preprocessor"
        <> progDesc
            "Generates a main module for a marvin project.\n\
            \Ignores original module contents. \
            \And thus also the input path argument. \
            \Automatically imports local scripts and scripts from external-scripts.json. \
            \Ignores scripts beginning with '_'. \
            \Configuration is either read from command line arguments or the config file \
            \(if provided)."
        )
    optsParser = Opts
        <$> optional
            (strOption
                $  long "adapter"
                <> short 'a'
                <> metavar "ID"
                <> help "adapter to use"
                <> showDefault
                <> completeWith (map fst adapters)
            )
        <*> argument str
            (  metavar "NAME"
            <> help "Name of the source file"
            )
        <*> argument str
            (  metavar "PATH"
            <> help "Path to the input file"
            )
        <*> argument str
            (  metavar "PATH"
            <> help "Path to the output file"
            )
        <*> strOption
            (  long "external-scripts"
            <> short 's'
            <> value "external-scripts.json"
            <> metavar "PATH"
            <> help "config file of external scripts to load"
            <> showDefault
            )
        <*> optional
            (strOption
            $  long "config-location"
            <> short 'c'
            <> metavar "PATH"
            <> help "config to use"
            <> showDefault
            <> value defaultConfigName
            )
