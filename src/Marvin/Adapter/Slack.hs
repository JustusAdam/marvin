{-# LANGUAGE TemplateHaskell #-}
module Marvin.Adapter.Slack (buildAdapter) where

import ClassyPrelude
import Data.Aeson hiding (Error)
import Data.Aeson.Types hiding (Error)
import Data.Aeson.TH
import Marvin.Adapter hiding (messageRoom, joinRoom, getUserInfo)
import Control.Monad
import Network.Wreq
import qualified Data.Configurator.Types as C
import qualified Data.Configurator as C
import Data.Time
import System.Log.Logger
import Network.WebSockets
import Marvin.Types
import Control.Lens hiding ((.=))
import Data.Aeson.Parser
import Network.URI
import Wuss
import qualified Data.ByteString.Lazy.Char8 as BS


data InternalType 
    = Error
        { code :: Int
        , msg :: Text
        }
    | Unhandeled Text
    | Ignored


instance FromJSON URI where
    parseJSON (String t) = maybe mzero return $ parseURI $ unpack t  
    parseJSON _ = mzero


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
    { ok :: Bool
    , url :: URI
    -- , self :: BotData
    }

data APIResponse a = APIResponse
    { responseOk :: Bool 
    , payload :: a
    }



(let opts = defaultOptions { fieldLabelModifier = camelTo2 '_' }
 in
     deriveJSON opts ''RTMData)


eventParser :: Value -> Parser (Either InternalType Event)
eventParser (Object o) = isErrParser <|> hasTypeParser
  where 
    isErrParser = do
        e <- o .: "error"
        case e of
            (Object eo) -> do
                ev <- Error <$> eo .: "code" <*> eo .: "msg"
                return $ Left ev
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


rawBS :: BS.ByteString -> String
rawBS bs = "\"" ++ BS.unpack bs ++ "\""


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
        _ -> mzero
userInfoParser _ = mzero


apiResponseParser :: (Value -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser f v@(Object o) = APIResponse <$> o .: "ok" <*> f v


runnerImpl :: C.Config -> EventHandler -> IO ()
runnerImpl cfg handler = do
    token <- C.require cfg "token"
    debugM "adapter.slack" "initializing socket"
    r <- post "https://slack.com/api/rtm.start" [ "token" := (token :: Text) ]
    case eitherDecode (r^.responseBody) of
        Left err -> errorM "adapter.slack" $ "Error decoding rtm json: " ++ err
        Right js -> do
            let uri = url js
                authority = fromMaybe (error "URI lacks authority") (uriAuthority uri)
                host = uriUserInfo authority ++ uriRegName authority 
                path = uriPath uri
                portOnErr v = do 
                    debugM "adapter.slack" $ "Unreadable port '" ++ v ++ "'"  
                    return 443
            port <- case uriPort authority of
                        v@(':':r) -> maybe (portOnErr v) return $ readMay r
                        v -> portOnErr v
            mids <- newMVar 0
            debugM "adapter.slack" $ "connecting to socket '" ++ show uri ++ "'"
            runSecureClient host port path $ \conn -> do
                debugM "adapter.slack" "Connection established"
                d <- receiveData conn
                case eitherDecode d >>= parseEither helloParser of
                    Right True -> debugM "adapter.slack" "Recieved hello packet"
                    Left _ -> error $ "Hello packet not readable: " ++ rawBS d
                    _ -> error $  "First packet was not hello packet: " ++ rawBS d
                forever $ do
                    let handlerImpl = handler (outputProviderImpl cfg conn mids)
                    d <- receiveData conn
                    case eitherDecode d >>= parseEither eventParser of
                        Left err -> errorM "adapter.slack" $ "Error parsing json: " ++ err ++ " original data: " ++ rawBS d
                        Right v -> 
                            case v of 
                                Right event -> handlerImpl event
                                Left internalEvent ->
                                    case internalEvent of
                                        Unhandeled type_ -> 
                                            debugM "adapter.slack" $ "Unhandeled event type " ++ unpack type_ ++ " payload " ++ rawBS d
                                        Error code msg -> 
                                            errorM "adapter.slack" $ "Error from remote code: " ++ show code ++ " msg: " ++ unpack msg
                                        Ignored -> return ()
    

execAPIMethod :: (Value -> Parser a) -> C.Config -> String -> [FormParam] -> IO (Either String (APIResponse a))
execAPIMethod innerParser cfg method params = do
    token <- C.require cfg "token"
    response <- post ("https://slack.com/api/" ++ method) (("token" := (token :: Text)):params)
    debugM "adapter.slack" (BS.unpack $ response^.responseBody)
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)


newMid :: MVar Int -> IO Int
newMid mids = do
    id <- takeMVar mids
    putMVar mids  (id + 1)
    return id


messageRoom :: MVar Int -> Connection -> Room -> Text -> IO ()
messageRoom mids conn (Room room) msg = do
    mid <- newMid mids
    sendTextData conn $ encode $ 
        object [ "id" .= mid 
                , "type" .= ("message" :: Text)
                , "channel" .= room
                , "text" .= msg  
                ]  


getUserInfo :: C.Config -> User -> IO (Maybe UserInfo)
getUserInfo cfg (User user) = do
    usr <- execAPIMethod userInfoParser cfg "users.info" ["user" := user]
    case usr of
        Left err -> errorM "adapter.slack" ("Parse error when getting user data " ++ err) >> return Nothing
        Right (APIResponse True v) -> return (Just v)
        Right (APIResponse False _) -> errorM "adapter.slack" "Server denied getting user info request" >> return Nothing



joinRoom :: C.Config -> Room -> IO ()
joinRoom config (Room room) = do
    response <- execAPIMethod (const $ return ()) config "channels.join" [ "name" := room ]
    case response of
        Left err -> errorM "adapter.slack" $ "Invalid json: " ++ err
        Right (APIResponse True ()) -> infoM "adapter.slack" $ "Successfully joined channel " ++ unpack room
        Right (APIResponse False ()) -> errorM "adapter.slack" $ "Failed to join channel " ++ unpack room



outputProviderImpl :: C.Config -> Connection -> MVar Int -> OutputProvider
outputProviderImpl cfg conn mids =
    OutputProvider (messageRoom mids conn) (joinRoom cfg) (getUserInfo cfg)



buildAdapter :: BuildAdapter
buildAdapter = BuildAdapter "slack-rtm" $ \cfg ->
    return $ Adapter (runnerImpl cfg)