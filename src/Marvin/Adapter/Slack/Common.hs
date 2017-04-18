{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import qualified Data.ByteString.Lazy            as B
import           Data.Char                       (isSpace)
import           Data.Foldable                   (asum)
import qualified Data.HashMap.Strict             as HM
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.Encoding         as L
import           Marvin.Adapter                  hiding (mkAdapterId)
import           Marvin.Adapter.Slack.Types
import           Marvin.Interpolate.All
import           Marvin.Types
import           Network.HTTP.Types.Status       (ok200)
import           Network.Wreq
import           Util


messageParser :: Value -> Parser (SlackUserId, SlackChannelId, UserInfo -> LimitedChannelInfo -> Event (SlackAdapter a))
messageParser = withObject "expected object" $ \o -> do
    messageContent <- o .: "text"
    ts <- o .: "ts" >>= timestampFromNumber
    (,,\u c -> MessageEvent u c messageContent ts) <$> o .: "user" <*> o .: "channel"


flip2_2 :: (a -> b -> c -> d -> e) -> c -> d -> a -> b -> e
flip2_2 f c d a b = f a b c d


flip2_1 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip2_1 f c a b = f a b c


eventParser :: MkSlack a => Value -> Parser (InternalType a)
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
            "error"             -> Error <$> o .: "code" <*> o .: "msg"
            "message"           -> messageTypeEvent
            "message.channels"  -> messageTypeEvent
            "message.groups"    -> messageTypeEvent
            "message.im"        -> messageTypeEvent
            "message.mpim"      -> messageTypeEvent
            "reconnect_url"     -> return Ignored
            "channel_archive"   -> ChannelArchiveStatusChange <$> o .: "channel" <*> pure True
            "channel_unarchive" -> ChannelArchiveStatusChange <$> o .: "channel" <*> pure False
            "channel_created"   -> ChannelCreated <$> (o .: "channel" >>= lciParser)
            "channel_deleted"   -> ChannelDeleted <$> o .: "channel"
            "channel_rename"    -> ChannelRename <$> (o .: "channel" >>= lciParser)
            "user_change"       -> UserChange <$> (o .: "user" >>= userInfoParser)
            _                   -> return $ Unhandeled t
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
                        wrapEv $ flip2_2 TopicChangeEvent <$> o .: "topic" <*> ts
                    "file_share" ->
                        wrapEv $ flip2_2 FileSharedEvent <$> o .: "file" <*> ts
                    _ -> msgEv

            _ -> msgEv
      where
        channel = o .: "channel"
        ts = o .: "ts" >>= timestampFromNumber
        msgEv = messageParser v
        wrapEv f = (,,) <$> o .: "user" <*> channel <*> f
        cJoin = wrapEv $ flip2_1 ChannelJoinEvent <$> ts
        cLeave = wrapEv $ flip2_1 ChannelLeaveEvent <$> ts
eventParser _ = fail "expected object"


stripWhiteSpaceMay :: L.Text -> Maybe L.Text
stripWhiteSpaceMay t =
    case L.uncons t of
        Just (c, _) | isSpace c -> Just $ L.stripStart t
        _           -> Nothing


runHandlerLoop :: MkSlack a => Chan (InternalType a) -> EventConsumer (SlackAdapter a) -> AdapterM (SlackAdapter a) ()
runHandlerLoop evChan handler =
    forever $ readChan evChan >>= \case
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


execAPIMethod :: MkSlack a => (Object -> Parser v) -> String -> [FormParam] -> AdapterM (SlackAdapter a) (Either L.Text v)
execAPIMethod innerParser method fparams = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post $(isS "https://slack.com/api/#{method}") (("token" := (token :: T.Text)):fparams)
    if response^.responseStatus == ok200
        then return $ mapLeft L.pack $ eitherDecode (response^.responseBody) >>= join . parseEither (apiResponseParser innerParser)
        else return $ Left $(isL "Recieved unexpected response from server: #{response^.responseStatus.statusMessage}")


execAPIMethodPart :: MkSlack a => (Object -> Parser v) -> String -> [Part] -> AdapterM (SlackAdapter a) (Either L.Text v)
execAPIMethodPart innerParser method formParts = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post $(isS "https://slack.com/api/#{method}") (partText "token" token:formParts)
    if response^.responseStatus == ok200
        then return $ mapLeft L.pack $ eitherDecode (response^.responseBody) >>= join . parseEither (apiResponseParser innerParser)
        else return $ Left $(isL "Recieved unexpected response from server: #{response^.responseStatus.statusMessage}")


messageChannelImpl :: SlackChannelId -> L.Text -> AdapterM (SlackAdapter a) ()
messageChannelImpl cid msg = do
    chan <- view $ adapter.outChannel
    writeChan chan (cid, msg)


getUserInfo :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
getUserInfo userid = do
    cache <- view $ adapter.userInfoCache
    uc <- readMVar cache
    maybe (refreshSingleUserInfo userid) return $ uc ^? infoCache. ix userid


getChannelInfo :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) LimitedChannelInfo
getChannelInfo cid = do
    cacheVar <- view $ adapter.channelCache
    cc <- readMVar cacheVar
    case cc ^? infoCache. ix cid of
        Nothing   -> refreshSingleChannelInfo cid
        Just info -> return info


refreshSingleUserInfo :: MkSlack a => SlackUserId -> AdapterM (SlackAdapter a) UserInfo
refreshSingleUserInfo userid@(SlackUserId user_) = do
    uc <- view $ adapter.userInfoCache
    execAPIMethod (\o -> o .: "user" >>= userInfoParser) "users.info" ["user" := user_] >>= \case
        Left err -> error $(isS "Parse error when getting data for user #{user_}: #{err}")
        Right v -> do
            modifyMVar_ uc (return . (infoCache . at userid .~ Just v))
            return v


refreshChannels :: MkSlack a => AdapterM (SlackAdapter a) (Either L.Text ChannelCache)
refreshChannels =
    execAPIMethod (\o -> o .: "channels" >>= lciListParser) "channels.list" [] >>= \case
        Left err -> return $ Left $(isL "Error when getting channel data: #{err}")
        Right v -> do
            let cmap = HM.fromList $ map ((^. idValue) &&& id) v
                nmap = HM.fromList $ map ((^. name) &&& id) v
                cache = ChannelCache cmap nmap
            return $ Right cache


refreshSingleChannelInfo :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) LimitedChannelInfo
refreshSingleChannelInfo chan@(SlackChannelId sid) =
    execAPIMethod (\o -> o .: "channel" >>= lciParser) "channels.info" ["channel" := sid] >>= \case
        Left err -> error $(isS "Error when getting channel data: #{err}")
        Right v -> do
            cache <- view $ adapter . channelCache
            modifyMVar_ cache (return . (infoCache . at chan .~ Just v))
            return v


resolveChannelImpl :: MkSlack a => L.Text -> AdapterM (SlackAdapter a) (Maybe LimitedChannelInfo)
resolveChannelImpl chanName = view (adapter.channelCache) >>= flip modifyMVar modifier
  where
    modifier cc =
        case cc ^? nameResolver . ix chanName of
            Nothing -> refreshChannels >>= \case
                Left err  -> logErrorN $(isT "#{err}") >> return (cc, Nothing)
                Right ncc -> return (ncc, ncc ^? nameResolver . ix chanName)
            Just found -> return (cc, Just found)


refreshUserInfo ::  MkSlack a => AdapterM (SlackAdapter a) (Either L.Text UserCache)
refreshUserInfo =
    execAPIMethod (\o -> o .: "members" >>= userInfoListParser) "users.list" [] >>= \case
        Left err -> return $ Left $(isL "Error when getting channel data: #{err}")
        Right v -> do
            let cmap = HM.fromList $ map ((^. idValue) &&& id) v
                nmap = HM.fromList $ map ((^. username) &&& id) v
                cache = UserCache cmap nmap
            return $ Right cache


resolveUserImpl :: MkSlack a => L.Text -> AdapterM (SlackAdapter a) (Maybe UserInfo)
resolveUserImpl uname = view (adapter.userInfoCache) >>= flip modifyMVar modifier
  where
    modifier uc =
        case uc ^? nameResolver . ix uname of
            Nothing -> refreshUserInfo >>= \case
                Left err  -> logErrorN $(isT "#{err}") >> return (uc, Nothing)
                Right newUc -> return (newUc, newUc ^? nameResolver . ix uname)
            Just found -> return (uc, Just found)


getChannelNameImpl :: MkSlack a => SlackChannelId -> AdapterM (SlackAdapter a) L.Text
getChannelNameImpl channel = view (adapter.channelCache) >>= readMVar >>= modifier
  where
    modifier cc =
        case cc ^? infoCache . ix channel of
            Nothing    -> (^.name) <$> refreshSingleChannelInfo channel
            Just found -> return $ found ^. name



putChannel :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
putChannel  channelInfo@(LimitedChannelInfo chanId chanName _) =
    view (adapter.channelCache) >>= flip modifyMVar_ (pure . modifier)
  where
    modifier =
        (infoCache . at chanId .~ Just channelInfo)
        . (nameResolver . at chanName .~ Just channelInfo)


deleteChannel :: SlackChannelId -> AdapterM (SlackAdapter a) ()
deleteChannel channel = view (adapter.channelCache) >>= flip modifyMVar_ (pure . modifier)
  where
    modifier cache =
        case cache ^? infoCache . ix channel of
            Nothing -> cache
            Just (LimitedChannelInfo _ chanName _) ->
                cache
                    & infoCache . at channel .~ Nothing
                    & nameResolver . at chanName .~ Nothing


renameChannel :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
renameChannel channelInfo@(LimitedChannelInfo cid chanName _) =
    view (adapter.channelCache) >>= flip modifyMVar_ (pure . modifier)
  where
    modifier cache = case cache ^? infoCache . ix cid of
        Just (LimitedChannelInfo _ oldName _) | oldName /= chanName ->
            inserted & nameResolver . at oldName .~ Nothing
        _ -> inserted
      where
        inserted = cache
                    & infoCache . at cid .~ Just channelInfo
                    & nameResolver . at chanName .~ Just channelInfo


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


partLText :: T.Text -> L.Text -> Part
partLText pName = partText pName . L.toStrict


instance MkSlack a => HasFiles (SlackAdapter a) where
    type RemoteFile (SlackAdapter a) = SlackRemoteFile a
    type LocalFile (SlackAdapter a) = SlackLocalFile

    readTextFile = fmap (fmap L.decodeUtf8) . readFileBytes
    readFileBytes file = do
        token <- requireFromAdapterConfig "token"
        r <- liftIO $ getWith (defaults & header "Authorization" .~ ["Bearer " `mappend` B.toStrict (L.encodeUtf8 token)]) (L.unpack $ file^.privateUrl)
        if r^.responseStatus == ok200
            then return $ Just $ r^.responseBody
            else do
                logErrorN $(isT "Unexpected status from server: #{r^.responseStatus.statusMessage}")
                return Nothing
    shareFile file channels =
        execAPIMethodPart (.: "file") "files.upload" $
            [ partLText "filename" $ file^.name
            , contentpart
            ]
            ++ maybe [] (pure . partLText "filetype") (file^.fileType)
            ++ maybe [] (pure . partLText "initial_comment") (file^.comment)
            ++ maybe [] (pure . partLText "title") (file^.title)
            ++ case channels of
                    [] -> []
                    a -> [partText "channels" $ T.intercalate "," $ map (unwrapSlackChannelId . (^.idValue)) a]
      where
        contentpart = case file^.content of
                FileOnDisk p       -> partFile "file" p
                FileInMemory bytes -> partLBS "file" bytes
    newLocalFile fname = return . SlackLocalFile fname Nothing Nothing Nothing

