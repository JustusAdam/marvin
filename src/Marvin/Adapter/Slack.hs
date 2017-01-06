{-|
Module      : $Header$
Description : Adapter for communicating with Slack via its real time event API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeSynonymInstances   #-}
module Marvin.Adapter.Slack (SlackRTMAdapter) where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   ((&&&))
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.Chan.Lifted  (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar.Lifted  (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar,
                                                  readMVar, takeMVar)
import           Control.Concurrent.STM          (TMVar, atomically, newTMVar, putTMVar, takeTMVar)
import           Control.Exception.Lifted
import           Control.Lens                    hiding ((.=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error)
import           Data.Aeson.TH
import           Data.Aeson.Types                hiding (Error)
import qualified Data.ByteString.Lazy.Char8      as BS
import qualified Data.Configurator               as C
import qualified Data.Configurator.Types         as C
import           Data.Containers
import           Data.Foldable                   (toList)
import           Data.HashMap.Strict             (HashMap)
import           Data.IORef.Lifted
import           Data.Maybe                      (fromMaybe)
import           Data.Sequences
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter
import           Marvin.Interpolate.Text
import           Marvin.Types                    as Types
import           Network.URI
import           Network.WebSockets
import           Network.Wreq
import           Prelude                         hiding (lookup)
import           Text.Read                       (readMaybe)
import           Wuss


instance FromJSON URI where
    parseJSON (String t) = maybe mzero return $ parseURI $ unpack t
    parseJSON _          = mzero


instance ToJSON URI where
    toJSON = toJSON . show


data RTMData = RTMData
    { ok  :: Bool
    , url :: URI
    -- , self :: BotData
    }

data APIResponse a = APIResponse
    { responseOk :: Bool
    , payload    :: a
    }


declareFields [d|
    data LimitedChannelInfo = LimitedChannelInfo
        { limitedChannelInfoIdValue :: Channel
        , limitedChannelInfoName :: L.Text
        , limitedChannelInfoTopic :: L.Text
        } deriving Show
    |]

declareFields [d|
    data UserInfo = UserInfo
        { userInfoUsername :: L.Text
        , userInfoIdValue  :: User
        }
    |]


declareFields [d|
    data ChannelCache = ChannelCache
        { channelCacheInfoCache    :: HashMap Channel LimitedChannelInfo
        , channelCacheNameResolver :: HashMap L.Text Channel
        }
    |]


data InternalType
    = Error
        { code :: Int
        , msg  :: String
        }
    | Unhandeled String
    | Ignored
    | ChannelArchiveStatusChange Channel Bool
    | ChannelCreated LimitedChannelInfo
    | ChannelDeleted Channel
    | ChannelRename LimitedChannelInfo
    | UserChange UserInfo


deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''RTMData


messageParser :: Value -> Parser Types.Message
messageParser (Object o) = Message
    <$> o .: "user"
    <*> o .: "channel"
    <*> o .: "text"
    <*> o .: "ts"
messageParser _ = mzero


eventParser :: Value -> Parser (Either InternalType Event)
eventParser v@(Object o) = isErrParser <|> hasTypeParser
  where
    isErrParser = do
        e <- o .: "error"
        case e of
            (Object eo) -> do
                ev <- Error <$> eo .: "code" <*> eo .: "msg"
                return $ Left ev
            _ -> mzero
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
                                t <- TopicChangeEvent <$> o .: "topic" <*> o .: "channel"
                                return $ Right t
                            _ -> msgEv

                    _ -> msgEv
              where
                msgEv = Right . MessageEvent <$> messageParser v
                cJoin = do
                    ev <- ChannelJoinEvent <$> o .: "user" <*> o .: "channel"
                    return $ Right ev
                cLeave = do
                    ev <- ChannelLeaveEvent <$> o .: "user" <*> o .: "channel"
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
eventParser _ = mzero


rawBS :: BS.ByteString -> String
rawBS bs = "\"" ++ BS.unpack bs ++ "\""


helloParser :: Value -> Parser Bool
helloParser (Object o) = do
    t <- o .: "type"
    return $ (t :: T.Text) == "hello"
helloParser _ = mzero


userInfoParser :: Value -> Parser UserInfo
userInfoParser (Object o) = do
    usr <- o .: "user"
    case usr of
        (Object o) -> UserInfo <$> o .: "name" <*> o .: "id"
        _          -> mzero
userInfoParser _ = mzero


apiResponseParser :: (Value -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser f v@(Object o) = APIResponse <$> o .: "ok" <*> f v
apiResponseParser _ _            = mzero

data SlackRTMAdapter = SlackRTMAdapter
    { sendMessage   :: BS.ByteString -> RunnerM ()
    , userConfig    :: C.Config
    , midTracker    :: TMVar Int
    , channelChache :: IORef ChannelCache
    , userInfoCache :: IORef (HashMap User UserInfo)
    }


runConnectionLoop :: C.Config -> Chan BS.ByteString -> MVar Connection -> RunnerM ()
runConnectionLoop cfg messageChan connTracker = forever $ do
    token <- liftIO $ C.require cfg "token"
    $logDebug "initializing socket"
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
            $logDebug $(isT "connecting to socket '#{uri}'")
            logFn <- askLoggerIO
            catch
                (liftIO $ runSecureClient host port path $ \conn -> flip runLoggingT logFn $ do
                    logInfoN "Connection established"
                    d <- liftIO $ receiveData conn
                    case eitherDecode d >>= parseEither helloParser of
                        Right True -> $logDebug "Recieved hello packet"
                        Left _ -> error $ "Hello packet not readable: " ++ BS.unpack d
                        _ -> error $ "First packet was not hello packet: " ++ BS.unpack d
                    putMVar connTracker conn
                    forever $ do
                        d <- liftIO $ receiveData conn
                        writeChan messageChan d)
                $ \e -> do
                    void $ takeMVar connTracker
                    logErrorN $(isT "#{e :: ConnectionException}")


runHandlerLoop :: SlackRTMAdapter -> Chan BS.ByteString -> EventHandler SlackRTMAdapter -> RunnerM ()
runHandlerLoop adapter messageChan handler =
    forever $ do
        d <- readChan messageChan
        case eitherDecode d >>= parseEither eventParser of
            Left err -> logErrorN $(isT "Error parsing json: #{err} original data: #{rawBS d}")
            Right v ->
                case v of
                    Right event -> liftIO $ handler event
                    Left internalEvent ->
                        case internalEvent of
                            Unhandeled type_ ->
                                $logDebug $(isT "Unhandeled event type #{type_} payload: #{rawBS d}")
                            Error code msg ->
                                logErrorN $(isT "Error from remote code: #{code} msg: #{msg}")
                            Ignored -> return ()
                            ChannelArchiveStatusChange _ _ ->
                                -- TODO implement once we track the archiving status
                                return ()
                            ChannelCreated info ->
                                putChannel adapter info
                            ChannelDeleted chan -> deleteChannel adapter chan
                            ChannelRename info -> renameChannel adapter info
                            UserChange ui -> void $ refreshUserInfo adapter (ui^.idValue)


sendMessageImpl :: MVar Connection -> BS.ByteString -> RunnerM ()
sendMessageImpl connTracker msg = go (3 :: Int)
  where
    go 0 = logErrorN "Connection error, quitting retry."
    go n =
        catch
            (do
                conn <- readMVar connTracker
                liftIO $ sendTextData conn msg)
            $ \e -> do
                logErrorN $(isT "#{e :: ConnectionException}")
                go (n-1)


runnerImpl :: RunWithAdapter SlackRTMAdapter
runnerImpl cfg handlerInit = do
    midTracker <- liftIO $ atomically $ newTMVar 0
    connTracker <- newEmptyMVar
    messageChan <- newChan
    let send = sendMessageImpl connTracker
    adapter <- SlackRTMAdapter send cfg midTracker <$> newIORef (ChannelCache mempty mempty) <*> newIORef mempty
    handler <- liftIO $ handlerInit adapter
    void $ async $ runConnectionLoop cfg messageChan connTracker
    runHandlerLoop adapter messageChan handler


execAPIMethod :: (Value -> Parser a) -> SlackRTMAdapter -> String -> [FormParam] -> RunnerM (Either String (APIResponse a))
execAPIMethod innerParser adapter method params = do
    token <- liftIO $ C.require cfg "token"
    response <- liftIO $ post ("https://slack.com/api/" ++ method) (("token" := (token :: T.Text)):params)
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)
  where
    cfg = userConfig adapter


newMid :: SlackRTMAdapter -> RunnerM Int
newMid SlackRTMAdapter{midTracker} = liftIO $ atomically $ do
    id <- takeTMVar midTracker
    putTMVar midTracker  (id + 1)
    return id


messageChannelImpl :: SlackRTMAdapter -> Channel -> L.Text -> RunnerM ()
messageChannelImpl adapter (Channel chan) msg = do
    mid <- newMid adapter
    sendMessage adapter $ encode $
        object [ "id" .= mid
                , "type" .= ("message" :: T.Text)
                , "channel" .= chan
                , "text" .= msg
                ]


getUserInfoImpl :: SlackRTMAdapter -> User -> RunnerM UserInfo
getUserInfoImpl adapter user@(User user') = do
    uc <- readIORef $ userInfoCache adapter
    maybe (refreshUserInfo adapter user) return $ lookup user uc


refreshUserInfo :: SlackRTMAdapter -> User -> RunnerM UserInfo
refreshUserInfo adapter user@(User user') = do
    usr <- execAPIMethod userInfoParser adapter "users.info" ["user" := user']
    case usr of
        Left err -> error ("Parse error when getting user data " ++ err)
        Right (APIResponse True v) -> do
            atomicModifyIORef (userInfoCache adapter) ((, ()) . insertMap user v)
            return v
        Right (APIResponse False _) -> error "Server denied getting user info request"


lciParser :: Value -> Parser LimitedChannelInfo
lciParser (Object o) = LimitedChannelInfo <$> o .: "id" <*> o .: "name" <*> (o .: "topic" >>= withObject "object" (.: "value"))
lciParser _ = mzero


lciListParser :: Value -> Parser [LimitedChannelInfo]
lciListParser = withArray "array" $ fmap toList . mapM lciParser


refreshChannels :: SlackRTMAdapter -> RunnerM (Either String ChannelCache)
refreshChannels adapter = do
    usr <- execAPIMethod (withObject "object" (\o -> o .: "channels" >>= lciListParser)) adapter "channels.list" []
    case usr of
        Left err -> return $ Left $ "Parse error when getting channel data " ++ err
        Right (APIResponse True v) -> do
            let cmap = mapFromList $ map ((^. idValue) &&& id) v
                nmap = mapFromList $ map ((^. name) &&& (^. idValue)) v
                cache = ChannelCache cmap nmap
            atomicWriteIORef (channelChache adapter) cache
            return $ Right cache
        Right (APIResponse False _) -> return $ Left "Server denied getting channel info request"


resolveChannelImpl :: SlackRTMAdapter -> L.Text -> RunnerM (Maybe Channel)
resolveChannelImpl adapter name' = do
    cc <- readIORef $ channelChache adapter
    case cc ^? nameResolver . ix name of
        Nothing -> do
            refreshed <- refreshChannels adapter
            case refreshed of
                Left err -> logErrorN $(isT "#{err}") >> return Nothing
                Right ncc -> return $ ncc ^? nameResolver . ix name
        Just found -> return (Just found)
  where name = L.tail name'


getChannelNameImpl :: SlackRTMAdapter -> Channel -> RunnerM L.Text
getChannelNameImpl adapter channel = do
    cc <- readIORef $ channelChache adapter
    L.cons '#' <$>
        case cc ^? infoCache . ix channel of
            Nothing -> do
                ncc <- either error id <$> refreshChannels adapter
                return $ (^.name) $ fromMaybe (error "Channel not found") $ ncc ^? infoCache . ix channel
            Just found -> return $ found ^. name


putChannel :: SlackRTMAdapter -> LimitedChannelInfo -> RunnerM ()
putChannel SlackRTMAdapter{channelChache} channelInfo@(LimitedChannelInfo id name _) =
    void $ atomicModifyIORef channelChache $ \cache ->
        (, ()) $ cache
                    & infoCache . at id .~ Just channelInfo
                    & nameResolver . at name .~ Just id


deleteChannel :: SlackRTMAdapter -> Channel -> RunnerM ()
deleteChannel SlackRTMAdapter{channelChache} channel =
    void $ atomicModifyIORef channelChache $ \cache ->
        case cache ^? infoCache . ix channel of
            Nothing -> (cache, ())
            Just (LimitedChannelInfo _ name _) ->
                (, ()) $ cache & infoCache . at channel .~ Nothing
                               & nameResolver . at name .~ Nothing


renameChannel :: SlackRTMAdapter -> LimitedChannelInfo -> RunnerM ()
renameChannel SlackRTMAdapter{channelChache} channelInfo@(LimitedChannelInfo id name _) =
    void $ atomicModifyIORef channelChache $ \cache ->
        let inserted = cache & infoCache . at id .~ Just channelInfo
                             & nameResolver . at name .~ Just id
        in case cache ^? infoCache . ix id of
               Just (LimitedChannelInfo _ oldName _) | oldName /= name ->
                   (, ()) $ inserted & nameResolver . at oldName .~ Nothing
               _ -> (inserted, ())



instance IsAdapter SlackRTMAdapter where
    adapterId = "slack-rtm"
    messageChannel = messageChannelImpl
    runWithAdapter = runnerImpl
    getUsername a = fmap (^.username) . getUserInfoImpl a
    getChannelName = getChannelNameImpl
    resolveChannel = resolveChannelImpl

