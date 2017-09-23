{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Arrow                (second, (***))
import           Control.Monad
import qualified Data.Char                    as Char
import           Data.Foldable                (for_)
import           Data.Maybe                   (isJust)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Options.Applicative
import           Paths_marvin
import           Prelude
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Mustache.Compile
import           Text.Mustache.Render
import           Text.Mustache.Types
import qualified Text.PrettyPrint.ANSI.Leijen as PP

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
    , ("package.yaml.mustache", "package.yaml")
    , ("bot.cabal.mustache", "{{ name }}.cabal")
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


itemize :: [PP.Doc] -> PP.Doc
itemize = PP.vsep . map ("-" PP.<+>)


autoText :: String -> PP.Doc
autoText = PP.fillSep . map PP.text . go id
  where
    go f l =
        let (prefix, rest) = break Char.isSpace l
        in case rest of
                _:r -> go (f . (prefix :)) r
                _   -> f [prefix]



main :: IO ()
main = do
    Opts{..} <- execParser infoParser
    d <- (</> "resources/initializer") <$> getDataDir
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
        (  fullDesc
        <> header "marvin-init ~ scaffolding for a new marvin project"
        <> (progDescDoc . Just)
            (      PP.text "Generate the files necessary to get started with a new marvin project."
            PP.<$> PP.text "This utility will generate:"
            PP.<$> (PP.indent 2 . itemize . map autoText)
                [ "A marvin configuration file (config.cfg)"
                , "A main file that automatically imports scripts using the preprocessor `marvin-pp`"
                , "A simple script to get started"
                , "A package.yaml file and a <bot>.cabal file to manage your dependencies"
                ]
            PP.<$> autoText "This program will"
                   PP.</> (PP.bold "not") PP.</>
                   autoText "overwrite an existing file. Therefore if you already have a file \
                       \with the same name as one of the generated ones that file will not get \
                       \generated!"
            )
        )
    optsParser = Opts
        <$> argument str
            (  metavar "BOTNAME"
            <> help "A name for your bot"
            )
        <*> strOption
            (  long "adapter"
            <> short 'a'
            <> metavar "ID"
            <> value "slack-rtm"
            <> help "id of the adapter to use" )
