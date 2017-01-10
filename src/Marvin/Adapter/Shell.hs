{-# LANGUAGE NamedFieldPuns #-}
module Marvin.Adapter.Shell where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Char                       (isSpace)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Data.Time.Clock                 (getCurrentTime)
import           Marvin.Adapter
import           Marvin.Internal                 (defaultBotName)
import           Marvin.Internal.Types
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text.Lazy
import           System.Console.Haskeline
import qualified Data.Configurator as C


data ShellAdapter = ShellAdapter
    { output :: MVar (Maybe L.Text)
    }


help :: L.Text
help = L.unlines
    [ "Available commands:"
    , ":? ~ print this help"
    , ":join <user> ~ make <user> join the channel"
    , ":join <user> <channel> ~ make <user> join the channel <channel>"
    , ":leave <user> ~ make <user> leave the channel"
    , ":leave <user> <channel> ~ make <user> leave the channel <channel>"
    , ":topic <...topic> ~ change the topic to <topic>"
    ]


instance IsAdapter ShellAdapter where
    type User ShellAdapter = L.Text
    type Channel ShellAdapter = L.Text

    adapterId = "shell"
    messageChannel _ chan = do 
        ShellAdapter{output} <- getAdapter
        putMVar output $ Just chan
    getUsername = return
    getChannelName = return
    resolveChannel = return . Just
    initAdapter = ShellAdapter <$> newEmptyMVar
    runWithAdapter handler = do
        bot <- fromMaybe defaultBotName <$> lookupFromAppConfig "name"
        histfile <- lookupFromAdapterConfig "history-file"
        ShellAdapter out <- getAdapter
        liftIO $ runInputT defaultSettings {historyFile=histfile} $ do
            outputStrLn "Type :? to see a a list of available commands"
            forever $ do
                input <- getInputLine $(isS "#{bot}> ")
                ts <- liftIO $ TimeStamp <$> getCurrentTime
                case input of
                    Nothing -> return ()
                    Just i -> do
                        let mtext = L.pack i
                        h <- liftIO $ async $ do
                            case L.words mtext of
                                [":?"] -> putMVar out $ Just help
                                [":join", user] -> handler $ ChannelJoinEvent user "shell" ts
                                [":join", user, chan] -> handler $ ChannelJoinEvent user chan ts
                                [":leave", user] -> handler $ ChannelLeaveEvent user "shell" ts
                                [":leave", user, chan] -> handler $ ChannelLeaveEvent user chan ts
                                (":topic":t) -> handler $ TopicChangeEvent "shell" "shell" (L.unwords t) ts
                                (x:_) | ":" `L.isPrefixOf` x ->
                                    putMVar out $ Just $(isL "Unknown command #{x} or unexpected arguments")
                                _ ->  -- handle message
                                    handler $ case L.stripPrefix bot $ L.stripStart mtext of
                                                Just cmd | fmap (isSpace . fst) (L.uncons cmd) == Just True ->
                                                    CommandEvent "shell" "shell" (L.stripStart cmd) ts
                                                _ -> MessageEvent "shell" "shell" mtext ts
                            putMVar out Nothing
                        whileJust_ (liftIO $ takeMVar out) $ outputStrLn . L.unpack
                        liftIO $ wait h
