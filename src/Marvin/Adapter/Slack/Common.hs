{-# LANGUAGE ScopedTypeVariables #-}
module Marvin.Adapter.Slack.Common where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   ((&&&))
import           Control.Concurrent.Async.Lifted (async, link)
import           Control.Concurrent.Chan.Lifted  (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar.Lifted  (modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Lens                    hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error)
import           Data.Aeson.Types                hiding (Error)
import           Data.Char                       (isSpace)
import           Data.Foldable                   (asum)
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter                  hiding (mkAdapterId)
import           Marvin.Adapter.Slack.Types
import           Marvin.Interpolate.All
import           Marvin.Types
import           Network.Wreq
import           Util


messageParser :: Value -> Parser (SlackUserId, SlackChannelId, UserInfo -> LimitedChannelInfo -> Event (SlackAdapter a))
messageParser = withObject "expected object" $ \o -> do
    content <- o .: "text"
    ts <- o .: "ts" >>= timestampFromNumber
    (,,\u c -> MessageEvent u c content ts) <$> o .: "user" <*> o .: "channel"


eventParser :: Value -> Parser (InternalType a)
eventParser v@(Object o) = isErrParser <|> isOkParser <|> hasTypeParser
  where
    isErrParser = do
        e <- o .: "error"
        flip (withObject "expected object") e $ \eo ->
            Error <$> eo .: "code" <*> eo .: "msg"
    isOkParser = do
        ok :: Bool <- o .: "ok"
        msg <- o .: "text"
        if ok then return $ OkResponseEvent msg else fail "expected ok response"
    hasTypeParser = do
        t <- o .: "type"

        -- https://api.slack.com/rtm
        case t of
            "error" -> Error <$> o .: "code" <*> o .: "msg"
            "message" -> messageTypeEvent
            "message.channels" -> messageTypeEvent
            "message.groups" -> messageTypeEvent
            "message.im" -> messageTypeEvent
            "message.mpim" -> messageTypeEvent
            "reconnect_url" -> return Ignored
            "channel_archive" -> ChannelArchiveStatusChange <$> o .: "channel" <*> pure True
            "channel_unarchive" -> ChannelArchiveStatusChange <$> o .: "channel" <*> pure False
            "channel_created" -> ChannelCreated <$> (o .: "channel" >>= lciParser)
            "channel_deleted" -> ChannelDeleted <$> o .: "channel"
            "channel_rename" -> ChannelRename <$> (o .: "channel" >>= lciParser)
            "user_change" -> UserChange <$> (o .: "user" >>= userInfoParser)
            _ -> return $ Unhandeled t
    messageTypeEvent = do
        subt <- o .:? "subtype"
        SlackEvent <$> case (subt :: Maybe T.Text) of
            Just str ->
                case str of
                    "channel_join" -> cJoin
                    "group_join" -> cJoin
                    "channel_leave" -> cLeave
                    "group_leave" -> cLeave
                    "channel_topic" ->
                        wrapEv $ (\topic ts u c -> TopicChangeEvent u c topic ts) <$> o .: "topic" <*> ts
                    "file_share" ->
                        wrapEv $ (\topic ts u c -> FileSharedEvent u c topic ts) <$> o .: "file" <*> ts
                    _ -> msgEv

            _ -> msgEv
      where
        user = o .: "user"
        channel = o .: "channel"
        ts = o .: "ts" >>= timestampFromNumber
        msgEv = messageParser v
        wrapEv f = (,,) <$> user <*> channel <*> f
        cJoin = wrapEv $ (\t u c -> ChannelJoinEvent u c t) <$> ts
        cLeave = wrapEv $ (\t u c -> ChannelLeaveEvent u c t) <$> ts
eventParser _ = fail "expected object"


stripWhiteSpaceMay :: L.Text -> Maybe L.Text
stripWhiteSpaceMay t =
    case L.uncons t of
        Just (c, _) | isSpace c -> Just $ L.stripStart t
        _ -> Nothing


runHandlerLoop :: MkSlack a => Chan (InternalType a) -> EventConsumer (SlackAdapter a) -> AdapterM (SlackAdapter a) ()
runHandlerLoop evChan handler =
    forever $ do
        d <- readChan evChan
        case d of
            SlackEvent (uid, cid, f) -> do
                uinfo <- getUserInfo uid
                lci <- getChannelInfo cid

                case f uinfo lci of
                    ev@(MessageEvent u c m t) -> do

                        botname <- L.toLower <$> getBotname
                        let strippedMsg = L.stripStart m
                        let lmsg = L.toLower strippedMsg
                        handler $ case asum $ map ((\prefix -> if prefix `L.isPrefixOf` lmsg then Just $ L.drop (L.length prefix) strippedMsg else Nothing) >=> stripWhiteSpaceMay) [botname, L.cons '@' botname, L.cons '/' botname] of
                            Nothing -> ev
                            Just m' -> CommandEvent u c m' t

                    ev -> handler ev
            Unhandeled type_ ->
                logDebugN $(isT "Unhandeled event type #{type_} payload")
            Error code msg ->
                logErrorN $(isT "Error from remote code: #{code} msg: #{msg}")
            Ignored -> return ()
            OkResponseEvent msg_ ->
                logDebugN $(isT "Message successfully sent: #{msg_}")
            ChannelArchiveStatusChange _ _ ->
                -- TODO implement once we track the archiving status
                return ()
            ChannelCreated info ->
                putChannel info
            ChannelDeleted chan -> deleteChannel chan
            ChannelRename info -> renameChannel info
            UserChange ui -> void $ refreshSingleUserInfo (ui^.idValue)


runnerImpl :: MkSlack a => EventConsumer (SlackAdapter a) -> AdapterM (SlackAdapter a) ()
runnerImpl handler = do
    messageChan <- newChan
    a <- async $ initIOConnections messageChan
    link a
    runHandlerLoop messageChan handler


execAPIMethod :: MkSlack a => (Object -> Parser v) -> String -> [FormParam] -> AdapterM (SlackAdapter a) (Either String v)
execAPIMethod innerParser method params = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post $(isS "https://slack.com/api/#{method}") (("token" := (token :: T.Text)):params)
    return $ eitherDecode (response^.responseBody) >>= join . parseEither (apiResponseParser innerParser)


messageChannelImpl :: SlackChannelId -> L.Text -> AdapterM (SlackAdapter a) ()
messageChannelImpl cid msg = do
    SlackAdapter{outChannel} <- getAdapter
    writeChan outChannel (cid, msg)


getUserInfo :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
getUserInfo user = do
    adapter <- getAdapter
    uc <- readMVar $ userInfoCache adapter
    maybe (refreshSingleUserInfo user) return $ uc ^? infoCache. ix user


getChannelInfo :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) LimitedChannelInfo
getChannelInfo cid = do
    adapter <- getAdapter
    cc <- readMVar $ channelCache adapter
    return $ cc ^?! infoCache. ix cid


refreshSingleUserInfo :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
refreshSingleUserInfo user@(SlackUserId user') = do
    adapter <- getAdapter
    usr <- execAPIMethod (\o -> o .: "user" >>= userInfoParser) "users.info" ["user" := user']
    case usr of
        Left err -> error ("Parse error when getting user data " ++ err)
        Right v -> do
            modifyMVar_ (userInfoCache adapter) (return . (infoCache . at user .~ Just v))
            return v


refreshChannels :: MkSlack a => AdapterM (SlackAdapter a) (Either String ChannelCache)
refreshChannels = do
    chans <- execAPIMethod (\o -> o .: "channels" >>= lciListParser) "channels.list" []
    case chans of
        Left err -> return $ Left $ "Error when getting channel data " ++ err
        Right v -> do
            let cmap = HM.fromList $ map ((^. idValue) &&& id) v
                nmap = HM.fromList $ map ((^. name) &&& id) v
                cache = ChannelCache cmap nmap
            return $ Right cache


refreshSingleChannelInfo :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) LimitedChannelInfo
refreshSingleChannelInfo chan = do
    res <- execAPIMethod (\o -> o .: "channel" >>= lciParser) "channels.info" []
    case res of
        Left err -> error $ "Parse error when getting channel data " ++ err
        Right v -> do
            adapter <- getAdapter
            modifyMVar_ (channelCache adapter) (return . (infoCache . at chan .~ Just v))
            return v


resolveChannelImpl :: MkSlack a => L.Text -> AdapterM (SlackAdapter a) (Maybe LimitedChannelInfo)
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
            let cmap = HM.fromList $ map ((^. idValue) &&& id) v
                nmap = HM.fromList $ map ((^. username) &&& id) v
                cache = UserCache cmap nmap
            return $ Right cache


resolveUserImpl :: MkSlack a => L.Text -> AdapterM (SlackAdapter a) (Maybe UserInfo)
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
            & nameResolver . at name .~ Just channelInfo


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
                                & nameResolver . at name .~ Just channelInfo
            in case cache ^? infoCache . ix id of
                Just (LimitedChannelInfo _ oldName _) | oldName /= name ->
                    inserted & nameResolver . at oldName .~ Nothing
                _ -> inserted


-- | Class to enable polymorphism for 'SlackAdapter' over the method used for retrieving updates. ('RTM' or 'EventsAPI')
class MkSlack a where
    mkAdapterId :: AdapterId (SlackAdapter a)
    initIOConnections :: Chan (InternalType a) -> AdapterM (SlackAdapter a) ()


instance MkSlack a => IsAdapter (SlackAdapter a) where
    type User (SlackAdapter a) = UserInfo
    type Channel (SlackAdapter a) = LimitedChannelInfo
    initAdapter = SlackAdapter
        <$> newMVar (ChannelCache mempty mempty)
        <*> newMVar (UserCache mempty mempty)
        <*> newChan
    adapterId = mkAdapterId
    messageChannel = messageChannelImpl . (^.idValue)
    runAdapter = runnerImpl
    resolveChannel = resolveChannelImpl
    resolveUser = resolveUserImpl


instance HasFiles (SlackAdapter a) where
    type RemoteFile (SlackAdapter a) = SlackRemoteFile a
    type LocalFile (SlackAdapter a) = SlackLocalFile

    readTextFile _ = do
        logErrorN "Reading file is not implemented"
        return Nothing
    readFileBytes _ = do
        logErrorN "Reading file is not implemented"
        return Nothing
    shareFile _ _ =
        logErrorN "Sharing file is not implemented"
    newLocalFile = return . SlackLocalFile Nothing Nothing

