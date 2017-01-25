{-|
Module      : $Header$
Description : Adapter for communicating with Slack via its real time messaging API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE NamedFieldPuns #-}
module Marvin.Adapter.Slack.RTM
    ( SlackAdapter, RTM
    , SlackUserId, SlackChannelId
    , MkSlack
    ) where


import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM          (TMVar, atomically, newTMVar, putTMVar, takeTMVar)
import           Control.Exception.Lifted
import           Control.Lens                    hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error)
import           Data.Aeson.Types                hiding (Error)
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import           Marvin.Adapter
import           Marvin.Adapter.Slack.Common
import           Marvin.Adapter.Slack.Types
import           Marvin.Internal
import           Marvin.Interpolate.Text
import           Network.URI
import           Network.WebSockets
import           Network.Wreq
import           Text.Read                       (readMaybe)
import           Wuss


runConnectionLoop :: Chan BS.ByteString -> MVar Connection -> AdapterM (SlackAdapter RTM) ()
runConnectionLoop messageChan connectionTracker = forever $ do
    token <- requireFromAdapterConfig "token"
    logDebugN "initializing socket"
    r <- liftIO $ post "https://slack.com/api/rtm.start" [ "token" := (token :: T.Text) ]
    case eitherDecode (r^.responseBody) of
        Left err -> logErrorN $(isT "Error decoding rtm json #{err}")
        Right js -> do
            let uri = url js
                authority = fromMaybe (error "URI lacks authority") (uriAuthority uri)
                host = uriUserInfo authority ++ uriRegName authority
                path = uriPath uri
                portOnErr v = do
                    logErrorN $(isT "Unreadable port #{v}")
                    return 443
            port <- case uriPort authority of
                        v@(':':r) -> maybe (portOnErr v) return $ readMaybe r
                        v         -> portOnErr v
            logDebugN $(isT "connecting to socket '#{uri}'")
            logFn <- askLoggerIO
            catch
                (liftIO $ runSecureClient host port path $ \conn -> flip runLoggingT logFn $ do
                    logInfoN "Connection established"
                    d <- liftIO $ receiveData conn
                    case eitherDecode d >>= parseEither helloParser of
                        Right True -> logDebugN "Recieved hello packet"
                        Left _ -> error $ "Hello packet not readable: " ++ BS.unpack d
                        _ -> error $ "First packet was not hello packet: " ++ BS.unpack d
                    putMVar connectionTracker conn
                    forever $ do
                        d <- liftIO $ receiveData conn
                        writeChan messageChan d)
                $ \e -> do
                    void $ takeMVar connectionTracker
                    logErrorN $(isT "#{e :: ConnectionException}")


senderLoop :: MVar Connection -> AdapterM (SlackAdapter a) ()
senderLoop connectionTracker = do
    SlackAdapter{outChannel} <- getAdapter
    midTracker <- liftIO $ atomically $ newTMVar (0 :: Int)
    forever $ do
        (SlackChannelId sid, msg) <- readChan outChannel
        mid <- liftIO $ atomically $ do
            id <- takeTMVar midTracker
            putTMVar midTracker  (id + 1)
            return id
        let encoded = encode $ object
                [ "id" .= mid
                , "type" .= ("message" :: T.Text)
                , "channel" .= sid
                , "text" .= msg
                ]

            go 0 = logErrorN "Connection error, quitting retry."
            go n =
                catch
                    (do
                        conn <- readMVar connectionTracker
                        liftIO $ sendTextData conn encoded)
                    $ \e -> do
                        logErrorN $(isT "#{e :: ConnectionException}")
                        go (n-1)
        go (3 :: Int)


-- | Recieve events by opening a websocket to the Real Time Messaging API
data RTM


instance MkSlack RTM where
    mkAdapterId = "slack-rtm"
    initIOConnections inChan = do
        connTracker <- newEmptyMVar
        async $ runConnectionLoop inChan connTracker
        senderLoop connTracker

