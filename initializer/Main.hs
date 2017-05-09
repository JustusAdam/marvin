{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Arrow         (second, (***))
import           Control.Monad
import           Data.Foldable         (for_)
import           Data.Maybe            (isJust)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Options.Applicative
import           Paths_marvin
import           Prelude
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


adType :: [(String, (String, String))]
adType =
    [ ("slack-rtm", ("Marvin.Adapter.Slack.RTM", "(SlackAdapter RTM)"))
    , ("slack-events", ("Marvin.Adapter.Slack.EventsAPI", "(SlackAdapter EventsAPI)"))
    , ("telegram-poll", ("Marvin.Adapter.Telegram.Poll", "(TelegramAdapter Poll)"))
    , ("telegram-push", ("Marvin.Adapter.Telegram.Push", "(TelegramAdapter Push)"))
    , ("shell", ("Marvin.Adapter.Shell", "ShellAdapter"))
    , ("irc", ("Marvin.Adapter.IRC", "IRCAdapter"))
    ]


main :: IO ()
main = do
    Opts{..} <- execParser infoParser
    d <- (</> "initializer") <$> getDataDir
    unless (isJust $ lookup adapter adType) $ hPutStrLn stderr "Unrecognized adapter"

    let (adModule, adSig) = maybe ("IsAdapter a => ScriptInit a", "") (("import " <>) *** ("ScriptInit " <>)) (lookup adapter adType)
        subsData = object [ "name" ~> botname
                          , "scriptsig" ~> adSig
                          , "adaptermod" ~> adModule
                          , "adapter" ~> adapter
                          ]

    for_ wantDirectories $ \dir -> do
        exists <- doesDirectoryExist dir
        unless exists (createDirectory dir)

    for_ wantFiles $ \(source, target) -> do
        let targetName = T.unpack $ substituteValue target subsData
        targetExists <- doesFileExist targetName
        if  | targetExists -> putStrLn $ "Skipping " ++ targetName ++ ". File already exists!"
            | ".mustache" == takeExtension source -> do
                tpl <- fromEither <$> automaticCompile [d] source
                T.writeFile targetName $ substituteValue tpl subsData
            | otherwise -> copyFile source targetName

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

