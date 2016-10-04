{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
import           ClassyPrelude
import           System.Directory
import           System.FilePath
import           Text.Mustache
import           Text.Mustache.Compile
import Options.Generic


data Opts = Opts 
    { adapter :: Maybe String 
    } deriving (Generic)


instance ParseRecord Opts


adapters :: [(String, (String, String))]
adapters =
    [("slack-rtm", ("Marvin.Adapter.Slack", "SlackRTMAdapter"))]


tpl :: Template
tpl = $(embedTemplate ["app"] "Main.mustache")


main :: IO ()
main = do
    args <- getArgs
    case args of
        (srcname': srcLoc: out:r) -> do
            let opts = getRecordPure r
                srcname = unpack srcname'
                dir = takeDirectory srcname
            paths <- filter (not . ((||) <$> isPrefixOf "_" <*> isPrefixOf ".")) <$> getDirectoryContents dir
            files <- filterM (doesFileExist . (dir </>)) paths
            let hsFiles = map dropExtensions $ filter (/= takeFileName srcname) $ filter ((`elem` [".hs", ".lhs"]) . takeExtension) files
                adapterType = opts >>= adapter >>= flip lookup adapters
                processed = substitute tpl (object [ "scripts" ~> intercalate ", " (map (++ ".script") hsFiles)
                                                    , "imports" ~> hsFiles
                                                    , "adapter-import" ~> map fst adapterType
                                                    , "adapter-type" ~> maybe "AddYourAdapterTypeHere" snd adapterType 
                                                    ])
            writeFile (unpack out) processed
        _ -> error "unexpected arguments"
