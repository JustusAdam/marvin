{-|
Module      : $Header$
Description : Adapter for communicating with a shell prompt.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE NamedFieldPuns #-}
module Marvin.Adapter.Shell (ShellAdapter) where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Char                       (isSpace)
import           Data.Maybe                      (fromJust)
import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.IO               as L
import           Data.Time.Clock                 (getCurrentTime)
import           Marvin.Adapter
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text.Lazy
import           Marvin.Types
import           System.Console.Haskeline


-- | Adapter for a shell prompt
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
    , ":file <path> ~ share file with <path> to <channel>"
    , ":file <channel> <path> ~ share file with <path> to <channel>"
    , ":file <user> <channel> <path> ~ share file with <path> to <channel>"
    ]


instance HasFiles ShellAdapter where
    type File ShellAdapter = L.Text
    getFileName = return
    getFileType = error "not implemented"
    getFileUrl = return
    readFileContents = liftIO . L.readFile . L.unpack
    getCreationDate = error "not implemented"
    getFileSize = error "not implemented"


instance IsAdapter ShellAdapter where
    -- | Stores the username
    type User ShellAdapter = L.Text
    -- | Stores channel name
    type Channel ShellAdapter = L.Text

    adapterId = "shell"
    messageChannel _ chan = do
        ShellAdapter{output} <- getAdapter
        putMVar output $ Just chan
    -- | Just returns the value again
    getUsername = return
    -- | Just returns the value again
    getChannelName = return
    -- | Just returns the value again
    resolveChannel = return . Just
    -- | Just returns the value again
    resolveUser = return . Just
    initAdapter = ShellAdapter <$> newEmptyMVar
    runWithAdapter handler = do
        bot <- getBotname
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
                                (":topic":t) -> handler $ TopicChangeEvent "shell" "shell" (fromJust $ L.stripPrefix ":topic" mtext) ts
                                [":file", chan, path] -> handler $ FileSharedEvent "shell" chan path ts
                                [":file", path] -> handler $ FileSharedEvent "shell" "shell" path ts
                                [":file", user, chan, path] -> handler $ FileSharedEvent user chan path ts
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
