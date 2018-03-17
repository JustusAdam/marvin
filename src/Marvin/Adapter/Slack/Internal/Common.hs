{-# OPTIONS_GHC -fno-warn-orphans #-}
module Marvin.Adapter.Slack.Internal.Common where


import           Control.Applicative                 ((<|>))
import           Control.Arrow                       ((&&&))
import           Control.Concurrent.Async.Lifted     (async, link)
import           Control.Concurrent.Chan.Lifted      (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar.Lifted      (modifyMVar, modifyMVar_, newMVar, readMVar)
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Logger
import           Data.Aeson                          hiding (Error)
import           Data.Aeson.Types                    hiding (Error)
import qualified Data.ByteString.Lazy                as B
import           Data.Char                           (isSpace)
import           Data.Foldable                       (asum)
import qualified Data.HashMap.Strict                 as HM
import           Data.Maybe
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as L
import qualified Data.Text.Lazy.Encoding             as L
import           Lens.Micro.Platform                 hiding ((.=))
import           Marvin.Adapter                      hiding (mkAdapterId)
import           Marvin.Adapter.Slack.Internal.Types
import           Marvin.Interpolate.All
import           Marvin.Types
import           Network.HTTP.Types.Status           (ok200)
import           Network.Wreq
import           Util


messageParser :: Value -> Parser (InternalType a)
messageParser = withObject "expected object" $ \o -> do
    messageContent <- o .: "text"
    ts <- o .: "ts" >>= timestampFromNumber
    SlackEvent
        <$> o .: "user"
        <*> o .: "channel"
        <*> pure (\u c -> MessageEvent u c messageContent ts)


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
        ok <- o .: "ok"
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
        case (subt :: Maybe T.Text) of
            Just "channel_join"  -> cJoin
            Just "group_join"    -> cJoin
            Just "channel_leave" -> cLeave
            Just "group_leave"   -> cLeave
            Just "channel_topic" -> wrapEv $ flip2_2 TopicChangeEvent <$> o .: "topic" <*> ts
            Just "file_share"    -> wrapEv $ flip2_2 FileSharedEvent <$> o .: "file" <*> ts
            _                    -> msgEv
      where
        channel = o .: "channel"
        ts = o .: "ts" >>= timestampFromNumber
        msgEv = messageParser v
        wrapEv f = SlackEvent <$> o .: "user" <*> channel <*> f
        cJoin = wrapEv $ flip2_1 ChannelJoinEvent <$> ts
        cLeave = wrapEv $ flip2_1 ChannelLeaveEvent <$> ts
eventParser _ = fail "expected object"


startsWith :: (Char -> Bool) -> L.Text -> Bool
startsWith f = maybe False (f . fst) . L.uncons


runHandlerLoop :: MkSlack a
               => Chan (InternalType a)
               -> EventConsumer (SlackAdapter a)
               -> AdapterM (SlackAdapter a) ()
runHandlerLoop evChan handler =
    forever $ do
        ac <- try $ readChan evChan >>= prepareAction

        case ac of
            Left e -> logDebugN $(isT "Uncaught exception when preparing handler: #{e :: SomeException}")
            Right Nothing -> return ()
            Right (Just h) -> h `catch` \e ->
                logErrorN $(isT "Uncaught exception when executing handler: #{e :: SomeException}")
  where
    prepareAction (SlackEvent uid cid f) = do
        uinfo <- getUserInfo uid
        lci <- getChannelInfo cid

        case f uinfo lci of
            ev@(MessageEvent u c m t) -> do

                botname <- L.toLower <$> getBotname
                let strippedMsg = L.stripStart m
                    lmsg = L.toLower strippedMsg
                    match prefix
                        | prefix `L.isPrefixOf` lmsg
                        && startsWith isSpace short = Just $ L.stripStart short
                        | otherwise = Nothing
                      where short = L.drop (L.length prefix) strippedMsg
                return $ Just $
                    handler $ case
                                asum $
                                    map match
                                    [botname, L.cons '@' botname, L.cons '/' botname]
                              of
                        Nothing -> ev
                        Just m' -> CommandEvent u c m' t

            ev -> return $ Just $ handler ev
    prepareAction ev = do
        case ev of
            Unhandeled type_ -> logDebugN $(isT "Unhandeled event type #{type_} payload")
            Error code msg -> logErrorN $(isT "Error from remote code: #{code} msg: #{msg}")
            Ignored -> return ()
            OkResponseEvent msg_ -> logDebugN $(isT "Message successfully sent: #{msg_}")
            ChannelArchiveStatusChange _ _ ->
                -- TODO implement once we track the archiving status
                return ()
            ChannelCreated info -> putChannel info
            ChannelDeleted chan -> do
                info <- deleteChannel chan
                logDebugN $(isT "Deleted channel #{info} as result of an event")
            ChannelRename info -> renameChannel info
            UserChange ui -> void $ refreshSingleUserInfo (ui^.idValue)
            SlackEvent{} -> error "impossible"
        return Nothing


runnerImpl :: MkSlack a => EventConsumer (SlackAdapter a) -> AdapterM (SlackAdapter a) ()
runnerImpl handler = do
    messageChan <- newChan
    a <- async $ initIOConnections messageChan
    link a
    runHandlerLoop messageChan handler


execAPIMethod :: MkSlack a
              => (Object -> Parser v)
              -> String
              -> [FormParam]
              -> AdapterM (SlackAdapter a) (Either L.Text v)
execAPIMethod innerParser method fparams = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post $(isS "https://slack.com/api/#{method}") (("token" := (token :: T.Text)):fparams)
    return $
        if response^.responseStatus == ok200 then
            mapLeft L.pack $ eitherDecode (response^.responseBody)
                                >>= join . parseEither (apiResponseParser innerParser)
        else
            Left $(isL "Recieved unexpected response from server: #{response^.responseStatus.statusMessage}")


execAPIMethodPart :: MkSlack a
                  => (Object -> Parser v)
                  -> String
                  -> [Part]
                  -> AdapterM (SlackAdapter a) (Either L.Text v)
execAPIMethodPart innerParser method formParts = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post $(isS "https://slack.com/api/#{method}") (partText "token" token:formParts)
    return $
        if response^.responseStatus == ok200 then
            mapLeft L.pack $ eitherDecode (response^.responseBody)
                                >>= join . parseEither (apiResponseParser innerParser)
        else
            Left $(isL "Recieved unexpected response from server: #{response^.responseStatus.statusMessage}")


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
        Nothing   -> maybe (error $(isS "Channel #{cid} does not exist")) return =<< refreshSingleChannelInfo cid
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
refreshChannels = runExceptT $ do
    chans <- fetchAccum Nothing
    let cmap = HM.fromList $ map ((^. idValue) &&& id) chans
        nmap = HM.fromList $ mapMaybe (\chan -> (,chan) <$> (chan ^. name)) chans
        cache = ChannelCache cmap nmap
    return cache
  where
    fetchAccum cursor =
        lift (execAPIMethod (\o -> (,) <$> (o .: "channels" >>= lciListParser)
                                       <*> getCursor o)
                "conversations.list"
                params')
                >>= \case
            Left err -> throwError $(isL "Error when getting channel data: #{err}")
            Right (v, Nothing) -> pure v
            Right (v, Just cursor') -> (v ++) <$> fetchAccum (Just cursor')
      where
        params' = maybe id ((:) . ("cursor":=)) (cursor :: Maybe T.Text)
            [ "exclude_archived" := ("true" :: T.Text)
            , "types" := ("public_channel,private_channel,im" :: T.Text)
            ]
    getCursor o = o .:? "response_metadata" >>= maybe (pure Nothing) (.:? "next_cursor")


updateChannelCache :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
updateChannelCache newVal = do
    cacheVar <- view $ adapter . channelCache
    let chan = newVal ^. idValue
    modifyMVar_ cacheVar $ pure . \caches ->
        let oldName = join $ caches ^? infoCache . ix chan . name in
        caches & maybe id (\name' -> nameResolver . at name' .~ Nothing) oldName
               & infoCache . at chan .~ Just newVal
               & maybe id (\name' -> nameResolver . at name' .~ Just newVal) (newVal^.name)


refreshSingleChannelInfo :: MkSlack a => SlackChannelId
                         -> AdapterM (SlackAdapter a) (Maybe LimitedChannelInfo)
refreshSingleChannelInfo chan@(SlackChannelId sid) = do
    execAPIMethod (\o -> o .: "channel" >>= lciParser) "conversations.info" ["channel" := sid] >>= \case
        Left err -> do
            oldInfo <- deleteChannel chan
            logDebugN $(isT "Failed to get channel data for #{sid}. \
                            \Error: #{err}. \
                            \Current cached version is #{oldInfo}. \
                            \Deleting cached version!")
            return Nothing
        Right v -> do
            updateChannelCache v
            return $ Just v


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


putChannel :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
putChannel  channelInfo =
    view (adapter.channelCache) >>= flip modifyMVar_ (pure . modifier)
  where
    modifier =
        (infoCache . at (channelInfo^.idValue) .~ Just channelInfo)
        . maybe id (\name' -> nameResolver . at name' .~ Just channelInfo) (channelInfo^.name)


deleteChannel :: SlackChannelId -> AdapterM (SlackAdapter a) (Maybe LimitedChannelInfo)
deleteChannel channel = view (adapter.channelCache) >>= flip modifyMVar (pure . modifier)
  where
    modifier cache =
        case cache ^? infoCache . ix channel of
            Nothing -> (cache, Nothing)
            Just oldChan ->
                (cache
                    & infoCache . at channel .~ Nothing
                    & maybe id (\name' -> nameResolver . at name' .~ Nothing) (oldChan^.name)
                , Just oldChan)


renameChannel :: LimitedChannelInfo -> AdapterM (SlackAdapter a) ()
renameChannel channelInfo = updateChannelCache channelInfo


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


instance MkSlack a => SupportsFiles (SlackAdapter a) where
    type RemoteFile (SlackAdapter a) = SlackRemoteFile a
    type LocalFile (SlackAdapter a) = SlackLocalFile

    readFileBytes file = do
        token <- requireFromAdapterConfig "token"
        r <- liftIO $ getWith (defaults & header "Authorization" .~
                                ["Bearer " `mappend` B.toStrict (L.encodeUtf8 token)])
                              (L.unpack $ file^.privateUrl)
        if r^.responseStatus == ok200 then
            return $ Just $ r^.responseBody
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
                    a -> [ partText "channels"
                             $ T.intercalate ","
                             $ map (unwrapSlackChannelId . (^.idValue)) a
                         ]
      where
        contentpart = case file^.content of
                FileOnDisk p       -> partFile "file" p
                FileInMemory bytes -> partLBS "file" bytes
    newLocalFile fname = return . SlackLocalFile fname Nothing Nothing Nothing
