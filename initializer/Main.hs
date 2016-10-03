{-# LANGUAGE DeriveGeneric #-}
module Main where

import ClassyPrelude
import Options.Generic
import Paths_marvin
import System.Directory
import System.FilePath
import Text.Mustache.Compile
import Text.Mustache.Render
import Text.Mustache.Types


data Opts = Opts 
    { botname :: String
    , adapter :: Maybe String
    } deriving (Generic)


instance ParseRecord Opts


fromEither :: Show a => Either a b -> b
fromEither (Left e) = error $ "Was left: " ++ show e
fromEither (Right v) = v


wantDirectories = 
    [ "bot" ]


wantFiles = map (second $ fromEither . compileTemplate "")
    [ ("Main.hs.mustache", "bot/Main.hs") 
    , ("MyScript.hs.mustache", "bot/MyScript.hs")
    , ("config.cfg.mustache", "config.cfg")
    , ("bot.cabal.mustache", "{{name}}.cabal")
    ]


adType :: [(String, String)]
adType =
    [ ("slack-rtm", "SlackRTMAdapter") ]


main :: IO ()
main = do
    opts <- getRecord "Marvin init"
    d <- (</> "initializer") <$> getDataDir
    unless ((== Just True) $ (`member` adType) <$> adapter opts) $ putStrLn "Unrecognized adapter"

    let subsData = object [ "name" ~> botname opts
                          , "scriptsig" ~> maybe "IsAdapter a => ScriptInit a" ("ScriptInit " ++) (adapter opts >>= flip lookup adType)
                          , "adapter" ~> adapter opts
                          ]

    for_ wantDirectories $ \dir -> do
        exists <- doesDirectoryExist dir
        unless exists (createDirectory dir)

    for_ wantFiles $ \(source, target) -> do
        let targetName = unpack $ substituteValue target subsData
        if ".mustache" == takeExtension source
            then do
                tpl <- fromEither <$> automaticCompile [d] source
                writeFile targetName $ substituteValue tpl subsData 
            else copyFile source targetName

    return ()