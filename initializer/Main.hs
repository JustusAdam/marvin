{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Arrow         (second)
import           Control.Monad
import           Data.Containers
import           Data.Foldable         (for_)
import           Data.Monoid           ((<>))
import           Data.Sequences
import qualified Data.Text.IO          as T
import           Options.Applicative
import           Paths_marvin
import           Prelude               hiding (lookup)
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Mustache.Compile
import           Text.Mustache.Render
import           Text.Mustache.Types


data Opts = Opts
    { botname :: String
    , adapter :: String
    }


fromEither :: Show a => Either a b -> b
fromEither (Left e)  = error $ "Was left: " <> show e
fromEither (Right v) = v


wantDirectories :: [FilePath]
wantDirectories =
    [ "bot" ]


wantFiles :: [(FilePath, Template)]
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
    Opts{..} <- execParser infoParser
    d <- (</> "initializer") <$> getDataDir
    unless (adapter `member` adType) $ hPutStrLn stderr "Unrecognized adapter"

    let subsData = object [ "name" ~> botname
                          , "scriptsig" ~> maybe "IsAdapter a => ScriptInit a" ("ScriptInit " <>) (lookup adapter adType)
                          , "adapter" ~> adapter
                          ]

    for_ wantDirectories $ \dir -> do
        exists <- doesDirectoryExist dir
        unless exists (createDirectory dir)

    for_ wantFiles $ \(source, target) -> do
        let targetName = unpack $ substituteValue target subsData
        if ".mustache" == takeExtension source
            then do
                tpl <- fromEither <$> automaticCompile [d] source
                T.writeFile targetName $ substituteValue tpl subsData
            else copyFile source targetName

    return ()
  where
    infoParser = info
        (helper <*> optsParser)
        (fullDesc <> header "marvin-init ~ make a new marvin project")
    optsParser = Opts
        <$> argument str (metavar "BOTNAME")
        <*> strOption
            (  long "adapter"
            <> short 'a'
            <> metavar "ID"
            <> value "slack-rtm"
            <> help "id of the adapter to use" )

