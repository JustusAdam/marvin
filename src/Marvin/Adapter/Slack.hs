{-|
Module      : $Header$
Description : Adapter for communicating with Slack via its real time event API and the Events API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NamedFieldPuns             #-}
module Marvin.Adapter.Slack
    ( SlackAdapter, RTM
    , SlackUserId, SlackChannelId
    , MkSlack
    ) where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   ((&&&))
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.Chan.Lifted  (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar.Lifted  (modifyMVar, modifyMVar_, newEmptyMVar,
                                                  newMVar, putMVar, readMVar, takeMVar)
import           Control.Concurrent.STM          (atomically, newTMVar, putTMVar, takeTMVar)
import           Control.Exception.Lifted
import           Control.Lens                    hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error)
import           Data.Aeson.Types                hiding (Error)
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Char                       (isSpace)
import           Data.Containers
import           Data.Foldable                   (asum)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter                  hiding (mkAdapterId)
import           Marvin.Adapter.Slack.Types
import           Marvin.Internal
import           Marvin.Internal.Types           as Types hiding (mkAdapterId)
import           Marvin.Interpolate.Text
import           Network.URI
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.WebSockets
import           Network.Wreq
import           Prelude                         hiding (lookup)
import           Text.Read                       (readMaybe)
import           Util
import           Wuss


messageParser :: Value -> Parser (Event (SlackAdapter a))
messageParser = withObject "expected object" $ \o ->
    MessageEvent
        <$> o .: "user"
        <*> o .: "channel"
        <*> o .: "text"
        <*> (o .: "ts" >>= timestampFromNumber)


eventParser :: Value -> Parser (Either InternalType (Event (SlackAdapter a)))
eventParser v@(Object o) = isErrParser <|> hasTypeParser
  where
    isErrParser = do
        e <- o .: "error"
        flip (withObject "expected object") e $ \eo ->
            Left <$> (Error <$> eo .: "code" <*> eo .: "msg")
    hasTypeParser = do
        t <- o .: "type"

        -- https://api.slack.com/rtm
        case t of
            "error" -> do
                ev <- Error <$> o .: "code" <*> o .: "msg"
                return $ Left ev
            "message" -> do
                subt <- o .:? "subtype"
                case (subt :: Maybe T.Text) of
                    Just str ->
                        case str of
                            "channel_join" -> cJoin
                            "group_join" -> cJoin
                            "channel_leave" -> cLeave
                            "group_leave" -> cLeave
                            "channel_topic" -> do
                                t <- TopicChangeEvent <$> user <*> channel <*> o .: "topic" <*> ts
                                return $ Right t
                            _ -> msgEv

                    _ -> msgEv
              where
                ts = o .: "ts" >>= timestampFromNumber
                msgEv = Right <$> messageParser v
                user = o .: "user"
                channel = o .: "channel"
                cJoin = do
                    ev <- ChannelJoinEvent <$> user <*> channel <*> ts
                    return $ Right ev
                cLeave = do
                    ev <- ChannelLeaveEvent <$> user <*> channel <*> ts
                    return $ Right ev
            "reconnect_url" -> return $ Left Ignored
            "channel_archive" -> do
                ev <- ChannelArchiveStatusChange <$> o .: "channel" <*> pure True
                return $ Left ev
            "channel_unarchive" -> do
                ev <- ChannelArchiveStatusChange <$> o .: "channel" <*> pure False
                return $ Left ev
            "channel_created" -> do
                ev <- o .: "channel" >>= lciParser
                return $ Left $ ChannelCreated ev
            "channel_deleted" -> Left . ChannelDeleted <$> o .: "channel"
            "channel_rename" -> do
                ev <- o .: "channel" >>= lciParser
                pure $ Left $ ChannelRename ev
            "user_change" -> do
                ev <- o .: "user" >>= userInfoParser
                pure $ Left $ UserChange ev
            _ -> return $ Left $ Unhandeled t
eventParser _ = fail "expected object"



runConnectionLoop :: Chan BS.ByteString -> AdapterM (SlackAdapter RTM) ()
runConnectionLoop messageChan = forever $ do
    SlackAdapter{connectionTracker} <- getAdapter
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


stripWhiteSpaceMay :: L.Text -> Maybe L.Text
stripWhiteSpaceMay t =
    case L.uncons t of
        Just (c, _) | isSpace c -> Just $ L.stripStart t
        _ -> Nothing


runHandlerLoop :: MkSlack a => Chan BS.ByteString -> EventHandler (SlackAdapter a) -> AdapterM (SlackAdapter a) ()
runHandlerLoop messageChan handler =
    forever $ do
        d <- readChan messageChan
        case eitherDecode d >>= parseEither eventParser of
            Left err -> logErrorN $(isT "Error parsing json: #{err} original data: #{rawBS d}")
            Right (Right ev@(MessageEvent u c m t)) -> do

                botname <- L.toLower <$> getBotname
                let lmsg = L.stripStart $ L.toLower m
                liftIO $ handler $ case asum $ map ((`L.stripPrefix` lmsg) >=> stripWhiteSpaceMay) [botname, L.cons '@' botname, L.cons '/' botname] of
                    Nothing -> ev
                    Just m' -> CommandEvent u c m' t

            Right (Right event) -> liftIO $ handler event
            Right (Left internalEvent) ->
                case internalEvent of
                    Unhandeled type_ ->
                        logDebugN $(isT "Unhandeled event type #{type_} payload: #{rawBS d}")
                    Error code msg ->
                        logErrorN $(isT "Error from remote code: #{code} msg: #{msg}")
                    Ignored -> return ()
                    ChannelArchiveStatusChange _ _ ->
                        -- TODO implement once we track the archiving status
                        return ()
                    ChannelCreated info ->
                        putChannel info
                    ChannelDeleted chan -> deleteChannel chan
                    ChannelRename info -> renameChannel info
                    UserChange ui -> void $ refreshSingleUserInfo (ui^.idValue)


sendMessageImpl :: BS.ByteString -> AdapterM (SlackAdapter a) ()
sendMessageImpl msg = do
    SlackAdapter{connectionTracker} <- getAdapter
    let go 0 = logErrorN "Connection error, quitting retry."
        go n =
            catch
                (do
                    conn <- readMVar connectionTracker
                    liftIO $ sendTextData conn msg)
                $ \e -> do
                    logErrorN $(isT "#{e :: ConnectionException}")
                    go (n-1)
    go (3 :: Int)



runnerImpl :: MkSlack a => RunWithAdapter (SlackAdapter a)
runnerImpl handler = do
    messageChan <- newChan
    void $ async $ mkEventGetter messageChan
    runHandlerLoop messageChan handler


execAPIMethod :: MkSlack a => (Object -> Parser v) -> String -> [FormParam] -> AdapterM (SlackAdapter a) (Either String v)
execAPIMethod innerParser method params = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post ("https://slack.com/api/" ++ method) (("token" := (token :: T.Text)):params)
    return $ eitherDecode (response^.responseBody) >>= join . parseEither (apiResponseParser innerParser)


newMid :: AdapterM (SlackAdapter a) Int
newMid = do
    SlackAdapter{midTracker} <- getAdapter
    liftIO $ atomically $ do
        id <- takeTMVar midTracker
        putTMVar midTracker  (id + 1)
        return id


messageChannelImpl :: SlackChannelId -> L.Text -> AdapterM (SlackAdapter a) ()
messageChannelImpl (SlackChannelId chan) msg = do
    mid <- newMid
    sendMessageImpl $ encode $ object
        [ "id" .= mid
        , "type" .= ("message" :: T.Text)
        , "channel" .= chan
        , "text" .= msg
        ]


getUserInfoImpl :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
getUserInfoImpl user = do
    adapter <- getAdapter
    uc <- readMVar $ userInfoCache adapter
    maybe (refreshSingleUserInfo user) return $ lookup user (uc^.infoCache)


refreshSingleUserInfo :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
refreshSingleUserInfo user@(SlackUserId user') = do
    adapter <- getAdapter
    usr <- execAPIMethod (\o -> o .: "user" >>= userInfoParser) "users.info" ["user" := user']
    case usr of
        Left err -> error ("Parse error when getting user data " ++ err)
        Right v -> do
            modifyMVar_ (userInfoCache adapter) (return . (infoCache %~ insertMap user v))
            return v


refreshChannels :: MkSlack a => AdapterM (SlackAdapter a) (Either String ChannelCache)
refreshChannels = do
    chans <- execAPIMethod (\o -> o .: "channels" >>= lciListParser) "channels.list" []
    case chans of
        Left err -> return $ Left $ "Error when getting channel data " ++ err
        Right v -> do
            let cmap = mapFromList $ map ((^. idValue) &&& id) v
                nmap = mapFromList $ map ((^. name) &&& (^. idValue)) v
                cache = ChannelCache cmap nmap
            return $ Right cache


refreshSingleChannelInfo :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) LimitedChannelInfo
refreshSingleChannelInfo chan = do
    res <- execAPIMethod (\o -> o .: "channel" >>= lciParser) "channels.info" []
    case res of
        Left err -> error $ "Parse error when getting channel data " ++ err
        Right v -> do
            adapter <- getAdapter
            modifyMVar_ (channelCache adapter) (return . (infoCache %~ insertMap chan v))
            return v            


resolveChannelImpl :: MkSlack a => L.Text -> AdapterM (SlackAdapter a) (Maybe SlackChannelId)
resolveChannelImpl name' = do
    adapter <- getAdapter
    modifyMVar (channelCache adapter) $ \cc ->
        case cc ^? nameResolver . ix name of
            Nothing -> do
                refreshed <- refreshChannels
                case refreshed of
                    Left err -> logErrorN $(isT "#{err}") >> return (cc, Nothing)
                    Right ncc -> return (ncc, ncc ^? nameResolver . ix name)
            Just found -> return (cc, Just found)
  where name = L.tail name'


refreshUserInfo ::  MkSlack a => AdapterM (SlackAdapter a) (Either String UserCache)
refreshUserInfo = do
    users <- execAPIMethod (\o -> o .: "members" >>= userInfoListParser) "users.list" []
    case users of
        Left err -> return $ Left $ "Error when getting channel data " ++ err
        Right v -> do
            let cmap = mapFromList $ map ((^. idValue) &&& id) v
                nmap = mapFromList $ map ((^. username) &&& (^. idValue)) v
                cache = UserCache cmap nmap
            return $ Right cache


resolveUserImpl :: MkSlack a => L.Text -> AdapterM (SlackAdapter a) (Maybe SlackUserId)
resolveUserImpl name = do
    adapter <- getAdapter
    modifyMVar (userInfoCache adapter) $ \cc ->
        case cc ^? nameResolver . ix name of
            Nothing -> do
                refreshed <- refreshUserInfo
                case refreshed of
                    Left err -> logErrorN $(isT "#{err}") >> return (cc, Nothing)
                    Right ncc -> return (ncc, ncc ^? nameResolver . ix name)
            Just found -> return (cc, Just found)


getChannelNameImpl :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) L.Text
getChannelNameImpl channel = do
    adapter <- getAdapter
    cc <- readMVar $ channelCache adapter
    L.cons '#' <$> 
        case cc ^? infoCache . ix channel of
            Nothing -> (^.name) <$> refreshSingleChannelInfo channel
            Just found -> return $ found ^. name



putChannel :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
putChannel  channelInfo@(LimitedChannelInfo id name _) = do
    SlackAdapter{channelCache} <- getAdapter
    modifyMVar_ channelCache $ \cache ->
        return $ cache
            & infoCache . at id .~ Just channelInfo
            & nameResolver . at name .~ Just id


deleteChannel :: SlackChannelId -> AdapterM (SlackAdapter a) ()
deleteChannel channel = do
    SlackAdapter{channelCache} <- getAdapter
    modifyMVar_ channelCache $ \cache ->
        return $ case cache ^? infoCache . ix channel of
            Nothing -> cache
            Just (LimitedChannelInfo _ name _) ->
                cache
                    & infoCache . at channel .~ Nothing
                    & nameResolver . at name .~ Nothing


renameChannel :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
renameChannel channelInfo@(LimitedChannelInfo id name _) = do
    SlackAdapter{channelCache} <- getAdapter
    modifyMVar_ channelCache $ \cache ->
        return $
            let inserted = cache
                                & infoCache . at id .~ Just channelInfo
                                & nameResolver . at name .~ Just id
            in case cache ^? infoCache . ix id of
                Just (LimitedChannelInfo _ oldName _) | oldName /= name ->
                    inserted & nameResolver . at oldName .~ Nothing
                _ -> inserted


class MkSlack a where
    mkAdapterId :: AdapterId (SlackAdapter a)
    mkEventGetter :: Chan BS.ByteString -> AdapterM (SlackAdapter a) ()



instance MkSlack RTM where
    mkAdapterId = "slack-rtm"
    mkEventGetter = runConnectionLoop


instance MkSlack EventsAPI where
    mkAdapterId = "slack-events"
    mkEventGetter = error "not implemented"


instance MkSlack a => IsAdapter (SlackAdapter a) where
    type User (SlackAdapter a) = SlackUserId
    type Channel (SlackAdapter a) = SlackChannelId
    initAdapter = SlackAdapter 
        <$> liftIO (atomically $ newTMVar 0)
        <*> newMVar (ChannelCache mempty mempty)
        <*> newMVar (UserCache mempty mempty) 
        <*> newEmptyMVar
    adapterId = mkAdapterId
    messageChannel = messageChannelImpl
    runWithAdapter = runnerImpl
    getUsername = fmap (^.username) . getUserInfoImpl
    getChannelName = getChannelNameImpl
    resolveChannel = resolveChannelImpl
    resolveUser = resolveUserImpl

