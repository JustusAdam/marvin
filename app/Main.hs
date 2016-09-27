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
        [srcname, srcLoc, out] -> do
            let dir = takeDirectory (unpack srcname)
            paths <- filter (not . ((||) <$> isPrefixOf "_" <*> isPrefixOf ".")) <$> getDirectoryContents dir
            files <- filterM doesFileExist paths
            let hsFiles = filter ((`elem` [".hs", ".lhs"]) . takeExtension) files
            writeFile (unpack out) (substitute tpl (object ["scripts" ~> hsFiles])) 
        _ -> error "unexpected arguments" 
