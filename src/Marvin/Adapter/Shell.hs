{-|
Module      : $Header$
Description : Adapter for communicating with a shell prompt.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Adapter.Shell (ShellAdapter) where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
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
    { output :: Chan L.Text
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
    getFileName = return . Just
    getFileType = error "not implemented"
    getFileUrl = return . Just
    readFileContents = fmap Just . liftIO . L.readFile . L.unpack
    getCreationDate = error "not implemented"
    getFileSize = error "not implemented"
    shareLocalFile path chans =
        forM_ chans $ \c -> messageChannel c $(isL "The bot has shared a file #{path} in this channel")


instance IsAdapter ShellAdapter where
    -- | Stores the username
    type User ShellAdapter = L.Text
    -- | Stores channel name
    type Channel ShellAdapter = L.Text

    adapterId = "shell"
    messageChannel _ msg = do
        ShellAdapter{output} <- getAdapter
        writeChan output msg
    -- | Just returns the value again
    getUsername = return
    -- | Just returns the value again
    getChannelName = return
    -- | Just returns the value again
    resolveChannel = return . Just
    -- | Just returns the value again
    resolveUser = return . Just
    initAdapter = ShellAdapter <$> newChan

    -- TODO test this output method actually works
    runAdapter handler = do
        bot <- getBotname
        histfile <- lookupFromAdapterConfig "history-file"
        ShellAdapter out <- getAdapter
        inChan <- newChan

        liftIO $ async $ forever $ readChan out >>= putStrLn . L.unpack
        async $ forever $ readChan inChan >>= handler

        liftIO $ runInputT defaultSettings {historyFile=histfile} $ do
            outputStrLn "Type :? to see a a list of available commands"

            forever $ do
                input <- getInputLine $(isS "#{bot}> ")
                ts <- liftIO $ TimeStamp <$> getCurrentTime
                case input of
                    Nothing -> return ()
                    Just i -> do
                        let mtext = L.pack i
                        liftIO $
                            case L.words mtext of
                                [":?"] -> writeChan out help
                                [":join", user] -> writeChan inChan $ ChannelJoinEvent user "shell" ts
                                [":join", user, chan] -> writeChan inChan $ ChannelJoinEvent user chan ts
                                [":leave", user] -> writeChan inChan $ ChannelLeaveEvent user "shell" ts
                                [":leave", user, chan] -> writeChan inChan $ ChannelLeaveEvent user chan ts
                                (":topic":t) -> writeChan inChan $ TopicChangeEvent "shell" "shell" (fromJust $ L.stripPrefix ":topic" mtext) ts
                                [":file", chan, path] -> writeChan inChan $ FileSharedEvent "shell" chan path ts
                                [":file", path] -> writeChan inChan $ FileSharedEvent "shell" "shell" path ts
                                [":file", user, chan, path] -> writeChan inChan $ FileSharedEvent user chan path ts
                                (x:_) | ":" `L.isPrefixOf` x ->
                                    writeChan out $(isL "Unknown command #{x} or unexpected arguments")
                                _ ->  -- handle message
                                    writeChan inChan $ case L.stripPrefix bot $ L.stripStart mtext of
                                                Just cmd | fmap (isSpace . fst) (L.uncons cmd) == Just True ->
                                                    CommandEvent "shell" "shell" (L.stripStart cmd) ts
                                                _ -> MessageEvent "shell" "shell" mtext ts
