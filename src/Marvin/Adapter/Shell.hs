{-|
Module      : $Header$
Description : Adapter for communicating with a shell prompt.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

See http://marvin.readthedocs.io/en/latest/adapters.html#shell for documentation of this adapter.
-}
module Marvin.Adapter.Shell (ShellAdapter) where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy            as B
import           Data.Char                       (isSpace)
import           Data.Maybe                      (fromJust)
import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.Encoding         as L
import qualified Data.Text.Lazy.IO               as L
import           Data.Time.Clock                 (getCurrentTime)
import           Marvin.Adapter
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text.Lazy
import           Marvin.Types
import           System.Console.Haskeline
import           System.Directory
import           System.FilePath
import           System.IO
import           Util


-- | Adapter for a shell prompt
declareFields [d|
    data ShellAdapter = ShellAdapter
        { shellAdapterOutput :: Chan L.Text
        -- , shellAdapterFiles :: IORef (HashMap L.Text ByteString)
        }
    |]


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


declareFields [d|
    data RFile = RFile
        { rFileName         :: Maybe L.Text
        , rFileFileType     :: Maybe L.Text
        , rFileSize         :: Integer
        , rFileCreationDate :: TimeStamp ShellAdapter
        , rFileContent      :: FileContent
        , rFileUrl          :: Maybe L.Text
        }
    data LFile = LFile
        { lFileName         :: L.Text
        , lFileFileType     :: Maybe L.Text
        , lFileCreationDate :: TimeStamp ShellAdapter
        , lFileContent      :: FileContent
        }
    |]


rFileFromLFile :: LFile -> AdapterM ShellAdapter RFile
rFileFromLFile (LFile fname type_ date fcontent) = do
    (fsize, furl) <- case fcontent of
               FileOnDisk path -> (,L.pack path) <$> sizeOfFile path
               FileInMemory bytes -> -- do
                --    fileCache <- view $ adapter . files
                --    atomicModifyIORef' fileCache $ at memurl .~ Just bytes
                   pure (fromIntegral $ B.length bytes, memurl)
                 where memurl = $(isL "memory://#{fname}")

    return $ RFile (Just fname) type_ fsize date fcontent (Just furl)


sizeOfFile :: MonadIO m => FilePath -> m Integer
sizeOfFile path = liftIO $ withFile path ReadMode hFileSize


instance HasFiles ShellAdapter where
    type RemoteFile ShellAdapter = RFile
    type LocalFile ShellAdapter = LFile
    newLocalFile fname fcontent = do
        t <- liftIO $ TimeStamp <$> case fcontent of
                FileOnDisk path -> getModificationTime path
                FileInMemory _  -> getCurrentTime
        return $ LFile fname Nothing t fcontent
    readTextFile f =
        case f^.content of
            FileOnDisk path    -> Just <$> liftIO (L.readFile path)
            FileInMemory bytes -> pure $ Just $ L.decodeUtf8 bytes
    readFileBytes f =
        case f^.content of
            FileOnDisk path    -> Just <$> liftIO (B.readFile path)
            FileInMemory bytes -> pure $ Just bytes
    shareFile file chans =
        mapM_ (`messageChannel` $(isL "The bot has shared a file #{file^.name} in this channel")) chans >> Right <$> rFileFromLFile file


pathToFile :: FilePath -> IO RFile
pathToFile path = do
    fileSize <- sizeOfFile path
    ctime <- liftIO $ getModificationTime path
    pure $ RFile (Just $ L.pack path) ftype fileSize (TimeStamp ctime) (FileOnDisk path) Nothing
  where ftype = case takeExtension path of "" -> Nothing; (_:a) -> Just $ L.pack a;


defaultUser :: SimpleWrappedUsername
defaultUser = "shell"


defaultChannel :: SimpleWrappedChannelName
defaultChannel = "shell"


instance IsAdapter ShellAdapter where
    -- | Stores the username
    type User ShellAdapter = SimpleWrappedUsername
    -- | Stores channel name
    type Channel ShellAdapter = SimpleWrappedChannelName

    adapterId = "shell"
    messageChannel _ msg = do
        o <- view (adapter . output)
        writeChan o msg
    -- | Just returns the value again
    resolveChannel = return . Just . SimpleWrappedChannelName
    -- | Just returns the value again
    resolveUser = return . Just . SimpleWrappedUsername
    initAdapter = ShellAdapter <$> newChan

    -- TODO test this output method actually works
    runAdapter handler = do
        bot <- getBotname
        histfile <- lookupFromAdapterConfig "history-file"
        ShellAdapter out <- getAdapter
        inChan <- newChan

        void $ liftIO $ async $ forever $ readChan out >>= L.putStrLn
        void $ async $ forever $ readChan inChan >>= handler

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
                                [":join", user] -> writeChan inChan $ ChannelJoinEvent (SimpleWrappedUsername user) defaultChannel ts
                                [":join", user, chan] -> writeChan inChan $ ChannelJoinEvent (SimpleWrappedUsername user) (SimpleWrappedChannelName chan) ts
                                [":leave", user] -> writeChan inChan $ ChannelLeaveEvent (SimpleWrappedUsername user) defaultChannel ts
                                [":leave", user, chan] -> writeChan inChan $ ChannelLeaveEvent (SimpleWrappedUsername user) (SimpleWrappedChannelName chan) ts
                                (":topic":_) -> writeChan inChan $ TopicChangeEvent defaultUser defaultChannel (L.stripStart $ fromJust $ L.stripPrefix ":topic" mtext) ts
                                [":file", chan, path] -> do
                                    f <- pathToFile $ L.unpack path
                                    writeChan inChan $ FileSharedEvent defaultUser (SimpleWrappedChannelName chan) f ts
                                [":file", path] -> do
                                    f <- pathToFile $ L.unpack path
                                    writeChan inChan $ FileSharedEvent defaultUser defaultChannel f ts
                                [":file", user, chan, path] -> do
                                    f <- pathToFile $ L.unpack path
                                    writeChan inChan $ FileSharedEvent (SimpleWrappedUsername user) (SimpleWrappedChannelName chan) f ts
                                (x:_) | ":" `L.isPrefixOf` x ->
                                    writeChan out $(isL "Unknown command #{x} or unexpected arguments")
                                _ ->  -- handle message
                                    writeChan inChan $ case L.stripPrefix bot $ L.stripStart mtext of
                                                Just cmd | fmap (isSpace . fst) (L.uncons cmd) == Just True ->
                                                    CommandEvent defaultUser defaultChannel (L.stripStart cmd) ts
                                                _ -> MessageEvent defaultUser defaultChannel mtext ts
