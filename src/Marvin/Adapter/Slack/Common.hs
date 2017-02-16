{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
module Marvin.Adapter.Slack.Common where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   ((&&&))
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.Chan.Lifted  (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar.Lifted  (modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Lens                    hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error)
import           Data.Aeson.Types                hiding (Error)
import qualified Data.ByteString.Lazy.Char8      as BS
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



messageParser :: Value -> Parser (Event (SlackAdapter a))
messageParser = withObject "expected object" $ \o ->
    MessageEvent
        <$> o .: "user"
        <*> o .: "channel"
        <*> o .: "text"
        <*> (o .: "ts" >>= timestampFromNumber)


eventParser :: Value -> Parser (InternalType a)
eventParser v@(Object o) = isErrParser <|> hasTypeParser
  where
    isErrParser = do
        e <- o .: "error"
        flip (withObject "expected object") e $ \eo ->
            Error <$> eo .: "code" <*> eo .: "msg"
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
                        TopicChangeEvent <$> user <*> channel <*> o .: "topic" <*> ts
                    _ -> msgEv

            _ -> msgEv
      where
        ts = o .: "ts" >>= timestampFromNumber
        msgEv = messageParser v
        user = o .: "user"
        channel = o .: "channel"
        cJoin = ChannelJoinEvent <$> user <*> channel <*> ts
        cLeave = ChannelLeaveEvent <$> user <*> channel <*> ts
eventParser _ = fail "expected object"


stripWhiteSpaceMay :: L.Text -> Maybe L.Text
stripWhiteSpaceMay t =
    case L.uncons t of
        Just (c, _) | isSpace c -> Just $ L.stripStart t
        _ -> Nothing


runHandlerLoop :: MkSlack a => Chan (InternalType a) -> EventHandler (SlackAdapter a) -> AdapterM (SlackAdapter a) ()
runHandlerLoop evChan handler =
    forever $ do
        d <- readChan evChan
        void $ async $ case d of
            SlackEvent ev@(MessageEvent u c m t) -> do

                botname <- L.toLower <$> getBotname
                let strippedMsg = L.stripStart m
                let lmsg = L.toLower strippedMsg
                liftIO $ handler $ case asum $ map ((\prefix -> if prefix `L.isPrefixOf` lmsg then Just $ L.drop (L.length prefix) strippedMsg else Nothing) >=> stripWhiteSpaceMay) [botname, L.cons '@' botname, L.cons '/' botname] of
                    Nothing -> ev
                    Just m' -> CommandEvent u c m' t

            SlackEvent event -> liftIO $ handler event
            Unhandeled type_ ->
                logDebugN $(isT "Unhandeled event type #{type_} payload")
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


runnerImpl :: MkSlack a => RunWithAdapter (SlackAdapter a)
runnerImpl handler = do
    messageChan <- newChan
    void $ async $ initIOConnections messageChan
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


getUserInfoImpl :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
getUserInfoImpl user = do
    adapter <- getAdapter
    uc <- readMVar $ userInfoCache adapter
    maybe (refreshSingleUserInfo user) return $ uc ^? infoCache. ix user


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
                nmap = HM.fromList $ map ((^. name) &&& (^. idValue)) v
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
            let cmap = HM.fromList $ map ((^. idValue) &&& id) v
                nmap = HM.fromList $ map ((^. username) &&& (^. idValue)) v
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


-- | Class to enable polymorphism for 'SlackAdapter' over the method used for retrieving updates. ('RTM' or 'EventsAPI')
class MkSlack a where
    mkAdapterId :: AdapterId (SlackAdapter a)
    initIOConnections :: Chan (InternalType a) -> AdapterM (SlackAdapter a) ()


instance MkSlack a => IsAdapter (SlackAdapter a) where
    type User (SlackAdapter a) = SlackUserId
    type Channel (SlackAdapter a) = SlackChannelId
    initAdapter = SlackAdapter
        <$> newMVar (ChannelCache mempty mempty)
        <*> newMVar (UserCache mempty mempty)
        <*> newChan
    adapterId = mkAdapterId
    messageChannel = messageChannelImpl
    runWithAdapter = runnerImpl
    getUsername = fmap (^.username) . getUserInfoImpl
    getChannelName = getChannelNameImpl
    resolveChannel = resolveChannelImpl
    resolveUser = resolveUserImpl

