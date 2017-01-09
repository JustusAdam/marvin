{-|
Module      : $Header$
Description : Adapter for communicating with Slack via its real time event API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Marvin.Adapter.Slack where


import           Control.Applicative             ((<|>))
import           Control.Arrow                   ((&&&))
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.Chan.Lifted  (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar.Lifted  (MVar, newEmptyMVar, putMVar, readMVar, takeMVar)
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
import           Data.Foldable                   (toList, asum)
import           Data.Hashable
import           Data.HashMap.Strict             (HashMap)
import           Data.IORef.Lifted
import           Data.Maybe                      (fromMaybe)
import           Data.Sequences
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter
import           Marvin.Interpolate.Text
import           Marvin.Types                    as Types
import           Network.URI
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.WebSockets
import           Network.Wreq
import           Prelude                         hiding (lookup)
import           Text.Read                       (readMaybe)
import           Wuss
import Marvin.Run
import Marvin.Internal
import Data.Char (isSpace)

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

-- | Identifier for a user (internal and not necessarily equal to the username)
newtype SlackUserId = SlackUserId T.Text deriving (IsString, Eq, Hashable)
-- | Identifier for a channel (internal and not necessarily equal to the channel name)
newtype SlackChannelId = SlackChannelId T.Text deriving (IsString, Eq, Show, Hashable)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''SlackUserId
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''SlackChannelId


declareFields [d|
    data LimitedChannelInfo = LimitedChannelInfo
        { limitedChannelInfoIdValue :: SlackChannelId
        , limitedChannelInfoName    :: L.Text
        , limitedChannelInfoTopic   :: L.Text
        } deriving Show
    |]

declareFields [d|
    data UserInfo = UserInfo
        { userInfoUsername :: L.Text
        , userInfoIdValue  :: SlackUserId
        }
    |]


declareFields [d|
    data ChannelCache = ChannelCache
        { channelCacheInfoCache    :: HashMap SlackChannelId LimitedChannelInfo
        , channelCacheNameResolver :: HashMap L.Text SlackChannelId
        }
    |]


data InternalType
    = Error
        { code :: Int
        , msg  :: String
        }
    | Unhandeled String
    | Ignored
    | ChannelArchiveStatusChange SlackChannelId Bool
    | ChannelCreated LimitedChannelInfo
    | ChannelDeleted SlackChannelId
    | ChannelRename LimitedChannelInfo
    | UserChange UserInfo


deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''RTMData


messageParser :: Value -> Parser (Event (SlackAdapter a))
messageParser (Object o) = MessageEvent
    <$> o .: "user"
    <*> o .: "channel"
    <*> o .: "text"
    <*> o .: "ts"
messageParser _ = mzero


eventParser :: Value -> Parser (Either InternalType (Event (SlackAdapter a)))
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
                                t <- TopicChangeEvent <$> user <*> channel <*> o .: "topic" <*> ts
                                return $ Right t
                            _ -> msgEv

                    _ -> msgEv
              where
                ts = o .: "ts"
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

data SlackAdapter a = SlackAdapter
    { sendMessage   :: BS.ByteString -> RunnerM ()
    , userConfig    :: C.Config
    , midTracker    :: TMVar Int
    , channelChache :: IORef ChannelCache
    , userInfoCache :: IORef (HashMap SlackUserId UserInfo)
    }


runConnectionLoop :: SlackAdapter RTM -> Chan BS.ByteString -> MVar Connection -> RunnerM ()
runConnectionLoop ada messageChan connTracker = forever $ do
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
  where cfg = userConfig ada


stripWhiteSpaceMay :: L.Text -> Maybe L.Text
stripWhiteSpaceMay t =
    case L.uncons t of
        Just (c, _) | isSpace c -> Just $ L.stripStart t
        _ -> Nothing


runHandlerLoop :: SlackAdapter a -> Chan BS.ByteString -> EventHandler (SlackAdapter a) -> RunnerM ()
runHandlerLoop adapter messageChan handler =
    forever $ do
        d <- readChan messageChan
        case eitherDecode d >>= parseEither eventParser of
            Left err -> logErrorN $(isT "Error parsing json: #{err} original data: #{rawBS d}")
            Right (Right ev@(MessageEvent u c m t)) -> do
                
                botname <- L.toLower . fromMaybe defaultBotName <$> liftIO (lookupFromAppConfig cfg "name")
                let lmsg = L.stripStart $ L.toLower m
                liftIO $ handler $ case asum $ map ((`L.stripPrefix` lmsg) >=> stripWhiteSpaceMay) [botname, L.cons '@' botname, L.cons '/' botname] of
                    Nothing -> ev
                    Just m' -> CommandEvent u c m' t
                
            Right (Right event) -> liftIO $ handler event
            Right (Left internalEvent) ->
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
  where
    cfg = userConfig adapter


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


runnerImpl :: MkSlack a => RunWithAdapter (SlackAdapter a)
runnerImpl cfg handlerInit = do
    midTracker <- liftIO $ atomically $ newTMVar 0
    connTracker <- newEmptyMVar
    messageChan <- newChan
    let send = sendMessageImpl connTracker
    adapter <- SlackAdapter send cfg midTracker <$> newIORef (ChannelCache mempty mempty) <*> newIORef mempty
    handler <- liftIO $ handlerInit adapter
    let eventGetter = mkEventGetter adapter
    void $ async $ eventGetter messageChan connTracker
    runHandlerLoop adapter messageChan handler


execAPIMethod :: (Value -> Parser v) -> SlackAdapter a -> String -> [FormParam] -> RunnerM (Either String (APIResponse v))
execAPIMethod innerParser adapter method params = do
    token <- liftIO $ C.require cfg "token"
    response <- liftIO $ post ("https://slack.com/api/" ++ method) (("token" := (token :: T.Text)):params)
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)
  where
    cfg = userConfig adapter


newMid :: SlackAdapter a -> RunnerM Int
newMid SlackAdapter{midTracker} = liftIO $ atomically $ do
    id <- takeTMVar midTracker
    putTMVar midTracker  (id + 1)
    return id


messageChannelImpl :: SlackAdapter a -> SlackChannelId -> L.Text -> RunnerM ()
messageChannelImpl adapter (SlackChannelId chan) msg = do
    mid <- newMid adapter
    sendMessage adapter $ encode $
        object [ "id" .= mid
                , "type" .= ("message" :: T.Text)
                , "channel" .= chan
                , "text" .= msg
                ]


getUserInfoImpl :: SlackAdapter a -> SlackUserId -> RunnerM UserInfo
getUserInfoImpl adapter user@(SlackUserId user') = do
    uc <- readIORef $ userInfoCache adapter
    maybe (refreshUserInfo adapter user) return $ lookup user uc


refreshUserInfo :: SlackAdapter a -> SlackUserId -> RunnerM UserInfo
refreshUserInfo adapter user@(SlackUserId user') = do
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


refreshChannels :: SlackAdapter a -> RunnerM (Either String ChannelCache)
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


resolveChannelImpl :: SlackAdapter a -> L.Text -> RunnerM (Maybe SlackChannelId)
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


getChannelNameImpl :: SlackAdapter a -> SlackChannelId -> RunnerM L.Text
getChannelNameImpl adapter channel = do
    cc <- readIORef $ channelChache adapter
    L.cons '#' <$>
        case cc ^? infoCache . ix channel of
            Nothing -> do
                ncc <- either error id <$> refreshChannels adapter
                return $ (^.name) $ fromMaybe (error "Channel not found") $ ncc ^? infoCache . ix channel
            Just found -> return $ found ^. name


putChannel :: SlackAdapter a -> LimitedChannelInfo -> RunnerM ()
putChannel SlackAdapter{channelChache} channelInfo@(LimitedChannelInfo id name _) =
    void $ atomicModifyIORef channelChache $ \cache ->
        (, ()) $ cache
                    & infoCache . at id .~ Just channelInfo
                    & nameResolver . at name .~ Just id


deleteChannel :: SlackAdapter a -> SlackChannelId -> RunnerM ()
deleteChannel SlackAdapter{channelChache} channel =
    void $ atomicModifyIORef channelChache $ \cache ->
        case cache ^? infoCache . ix channel of
            Nothing -> (cache, ())
            Just (LimitedChannelInfo _ name _) ->
                (, ()) $ cache & infoCache . at channel .~ Nothing
                               & nameResolver . at name .~ Nothing


renameChannel :: SlackAdapter a -> LimitedChannelInfo -> RunnerM ()
renameChannel SlackAdapter{channelChache} channelInfo@(LimitedChannelInfo id name _) =
    void $ atomicModifyIORef channelChache $ \cache ->
        let inserted = cache & infoCache . at id .~ Just channelInfo
                             & nameResolver . at name .~ Just id
        in case cache ^? infoCache . ix id of
               Just (LimitedChannelInfo _ oldName _) | oldName /= name ->
                   (, ()) $ inserted & nameResolver . at oldName .~ Nothing
               _ -> (inserted, ())


class MkSlack a where
    mkAdapterId :: SlackAdapter a -> AdapterId (SlackAdapter a)
    mkEventGetter :: SlackAdapter a -> Chan BS.ByteString -> MVar Connection -> RunnerM ()


data RTM


instance MkSlack RTM where
    mkAdapterId _ = "slack-rtm"
    mkEventGetter = runConnectionLoop


data EventsAPI


instance MkSlack EventsAPI where
    mkAdapterId _ = "slack-events"
    mkEventGetter = error "not implemented"


instance MkSlack a => IsAdapter (SlackAdapter a) where
    type User (SlackAdapter a) = SlackUserId
    type Channel (SlackAdapter a) = SlackChannelId
    adapterId = mkAdapterId (error "phantom value" :: SlackAdapter a)
    messageChannel = messageChannelImpl
    runWithAdapter = runnerImpl
    getUsername a = fmap (^.username) . getUserInfoImpl a
    getChannelName = getChannelNameImpl
    resolveChannel = resolveChannelImpl

