{-# LANGUAGE TemplateHaskell #-}
import           ClassyPrelude
import Text.Mustache
import Text.Mustache.Compile
import System.Directory
import System.FilePath


tpl :: Template
tpl = $(embedTemplate ["app"] "Main.mustache")


main :: IO ()
main = do 
    args <- getArgs
    case args of
        [srcname', srcLoc, out] -> do
            let srcname = unpack srcname' 
            let dir = takeDirectory srcname
            paths <- filter (not . ((||) <$> isPrefixOf "_" <*> isPrefixOf ".")) <$> getDirectoryContents dir
            files <- filterM (doesFileExist . (dir </>)) paths
            let hsFiles = map dropExtensions $ filter (/= takeFileName srcname) $ filter ((`elem` [".hs", ".lhs"]) . takeExtension) files
            let processed = (substitute tpl (object [ "scripts" ~> intercalate ", " (map (++ ".script") hsFiles)
                                                    , "imports" ~> hsFiles
                                                    ]))
            putStrLn processed
            writeFile (unpack out) processed  
        _ -> error "unexpected arguments" 
