{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Marvin.Adapter.Slack (SlackRTMAdapter) where

import           ClassyPrelude
import           Control.Lens               hiding ((.=))
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.TH
import           Data.Aeson.Types           hiding (Error)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Configurator          as C
import qualified Data.Configurator.Types    as C
import           Marvin.Adapter
import           Marvin.Types
import           Network.URI
import           Network.WebSockets
import           Network.Wreq
import           Wuss


data InternalType
    = Error
        { code :: Int
        , msg  :: Text
        }
    | Unhandeled Text
    | Ignored


instance FromJSON URI where
    parseJSON (String t) = maybe mzero return $ parseURI $ unpack t
    parseJSON _          = mzero


instance ToJSON URI where
    toJSON = toJSON . show



-- data BotData = BotData
--     { botId :: Text
--     , notName :: Text
--     , botCreated :: UTCTime
--     }

-- {
--     "ok": true,
--     "url": "wss:\/\/ms9.slack-msgs.com\/websocket\/7I5yBpcvk",

--     "self": {
--         "id": "U023BECGF",
--         "name": "bobby",
--         "prefs": {
--             …
--         },
--         "created": 1402463766,
--         "manual_presence": "active"
--     },
--     "team": {
--         "id": "T024BE7LD",
--         "name": "Example Team",
--         "email_domain": "",
--         "domain": "example",
--         "icon": {
--             …
--         },
--         "msg_edit_window_mins": -1,
--         "over_storage_limit": false
--         "prefs": {
--             …
--         },
--         "plan": "std"
--     },
--     "users": [ … ],

--     "channels": [ … ],
--     "groups": [ … ],
--     "mpims": [ … ],
--     "ims": [ … ],

--     "bots": [ … ],
-- }
data RTMData = RTMData
    { ok  :: Bool
    , url :: URI
    -- , self :: BotData
    }

data APIResponse a = APIResponse
    { responseOk :: Bool
    , payload    :: a
    }


deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' } ''RTMData


eventParser :: Value -> Parser (Either InternalType Event)
eventParser (Object o) = isErrParser <|> hasTypeParser
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

        case t of
            "error" -> do
                ev <- Error <$> o .: "code" <*> o .: "msg"
                return $ Left ev
            "message" -> do
                ev <- Message
                        <$> o .: "user"
                        <*> o .: "channel"
                        <*> o .: "text"
                        <*> o .: "ts"
                return $ Right (MessageEvent ev)
            "reconnect_url" -> return $ Left Ignored
            _ -> return $ Left $ Unhandeled t
eventParser _ = mzero


rawBS :: BS.ByteString -> Text
rawBS bs = "\"" ++ toStrict (decodeUtf8 bs) ++ "\""


showt :: Show a => a -> Text
showt = pack . show


helloParser :: Value -> Parser Bool
helloParser (Object o) = do
    t <- o .: "type"
    return $ (t :: Text) == "hello"
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


runConnectionLoop :: C.Config -> MVar BS.ByteString -> MVar Connection -> IO ()
runConnectionLoop cfg messageChan connTracker = forever $ do
    token <- C.require cfg "token"
    debugM pa "initializing socket"
    r <- post "https://slack.com/api/rtm.start" [ "token" := (token :: Text) ]
    case eitherDecode (r^.responseBody) of
        Left err -> errorM pa $ "Error decoding rtm json: " ++ pack err
        Right js -> do
            let uri = url js
                authority = fromMaybe (error "URI lacks authority") (uriAuthority uri)
                host = uriUserInfo authority ++ uriRegName authority
                path = uriPath uri
                portOnErr v = do
                    debugM pa $ "Unreadable port '" ++ pack v ++ "'"
                    return 443
            port <- case uriPort authority of
                        v@(':':r) -> maybe (portOnErr v) return $ readMay r
                        v         -> portOnErr v
            debugM pa $ "connecting to socket '" ++ showt uri ++ "'"
            catch
                (runSecureClient host port path $ \conn -> do
                    debugM pa "Connection established"
                    d <- receiveData conn
                    case eitherDecode d >>= parseEither helloParser of
                        Right True -> debugM pa "Recieved hello packet"
                        Left _ -> error $ "Hello packet not readable: " ++ BS.unpack d
                        _ -> error $ "First packet was not hello packet: " ++ BS.unpack d
                    putMVar connTracker conn
                    forever $ do
                        d <- receiveData conn
                        putMVar messageChan d)
                $ \e -> do
                    void $ takeMVar connTracker
                    errorM pa (pack $ show (e :: ConnectionException))
  where
    pa = error "Phantom value" :: SlackRTMAdapter


runHandlerLoop :: SlackRTMAdapter -> MVar BS.ByteString -> EventHandler SlackRTMAdapter -> IO ()
runHandlerLoop adapter messageChan handler =
    forever $ do
        d <- takeMVar messageChan
        case eitherDecode d >>= parseEither eventParser of
            Left err -> errorM adapter $ "Error parsing json: " ++ pack err ++ " original data: " ++ rawBS d
            Right v ->
                case v of
                    Right event -> handler event
                    Left internalEvent ->
                        case internalEvent of
                            Unhandeled type_ ->
                                debugM adapter $ "Unhandeled event type " ++ type_ ++ " payload " ++ rawBS d
                            Error code msg ->
                                errorM adapter $ "Error from remote code: " ++ showt code ++ " msg: " ++ msg
                            Ignored -> return ()


runnerImpl :: RunWithAdapter SlackRTMAdapter
runnerImpl cfg handlerInit = do
    midTracker <- newMVar 0
    connTracker <- newEmptyMVar
    messageChan <- newEmptyMVar
    let send d = do
            conn <- readMVar connTracker
            sendTextData conn d
        adapter = SlackRTMAdapter send cfg midTracker
    handler <- handlerInit adapter
    void $ async $ runConnectionLoop cfg messageChan connTracker
    runHandlerLoop adapter messageChan handler


execAPIMethod :: (Value -> Parser a) -> SlackRTMAdapter -> String -> [FormParam] -> IO (Either String (APIResponse a))
execAPIMethod innerParser adapter method params = do
    token <- C.require cfg "token"
    response <- post ("https://slack.com/api/" ++ method) (("token" := (token :: Text)):params)
    debugM adapter (toStrict $ decodeUtf8 $ response^.responseBody)
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)
  where
    cfg = userConfig adapter


newMid :: SlackRTMAdapter -> IO Int
newMid SlackRTMAdapter{midTracker} = do
    id <- takeMVar midTracker
    putMVar midTracker  (id + 1)
    return id


messageRoomImpl :: SlackRTMAdapter -> Room -> Text -> IO ()
messageRoomImpl adapter (Room room) msg = do
    mid <- newMid adapter
    sendMessage adapter $ encode $
        object [ "id" .= mid
                , "type" .= ("message" :: Text)
                , "channel" .= room
                , "text" .= msg
                ]


getUserInfoImpl :: SlackRTMAdapter -> User -> IO (Maybe UserInfo)
getUserInfoImpl adapter (User user) = do
    usr <- execAPIMethod userInfoParser adapter "users.info" ["user" := user]
    case usr of
        Left err -> errorM adapter ("Parse error when getting user data " ++ pack err) >> return Nothing
        Right (APIResponse True v) -> return (Just v)
        Right (APIResponse False _) -> errorM adapter "Server denied getting user info request" >> return Nothing


data SlackRTMAdapter = SlackRTMAdapter
    { sendMessage :: BS.ByteString -> IO ()
    , userConfig  :: C.Config
    , midTracker  :: MVar Int
    }


instance IsAdapter SlackRTMAdapter where
    adapterId = "slack-rtm"
    messageRoom = messageRoomImpl
    getUserInfo = getUserInfoImpl
    runWithAdapter = runnerImpl
