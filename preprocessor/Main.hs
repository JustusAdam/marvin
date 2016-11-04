{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
import           ClassyPrelude
import           System.Directory
import           System.FilePath
import           Text.Mustache
import           Text.Mustache.Compile
import           Options.Applicative


data Opts = Opts 
    { adapter :: String
    , sourceName :: FilePath
    , sourceLocation :: FilePath
    , targetFile :: FilePath 
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
    let dir = takeDirectory sourceName
    paths <- filter (not . ((||) <$> isPrefixOf "_" <*> isPrefixOf ".")) <$> getDirectoryContents dir
    files <- filterM (doesFileExist . (dir </>)) paths
    let hsFiles = map dropExtensions $ filter (/= takeFileName sourceName) $ filter ((`elem` [".hs", ".lhs"]) . takeExtension) files
        (adapterImport, adapterType) = fromMaybe slackRtmData $ lookup adapter adapters
        processed = substitute tpl (object [ "scripts" ~> intercalate ", " (map (++ ".script") hsFiles)
                                            , "imports" ~> hsFiles
                                            , "adapter-import" ~> adapterImport
                                            , "adapter-type" ~> adapterType 
                                            ])
    writeFile targetFile processed
  where
    infoParser = info 
        (helper <*> optsParser) 
        (fullDesc ++ header "marvin-pp ~ the marvin preprocessor")
    optsParser = Opts
        <$> strOption
            (  long "adapter"
            ++ short 'a'
            ++ value "slack-rtm"
            ++ metavar "ID"
            ++ help "adapter to use"
            ++ showDefault
            )
        <*> argument str (metavar "NAME")
        <*> argument str (metavar "PATH")
        <*> argument str (metavar "PATH")
