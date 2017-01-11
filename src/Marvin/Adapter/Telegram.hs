{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Marvin.Adapter.Telegram
    ( TelegramAdapter, Push, Poll
    , TelegramChat, ChatType(..)
    , TelegramUser
    , MkTelegram()
    --, HasId_(id_), HasUsername(username), HasFirstName(firstName), HasLastName(lastName), HasType_(type_)
    ) where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error, Success)
import           Data.Aeson.Types                (Parser, parseEither)
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter
import           Marvin.Internal.Types
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wreq
import           Util


data APIResponse a
    = Success { description :: Maybe T.Text, result :: a}
    | Error { errorCode :: Int, errDescription :: T.Text}


data TelegramAdapter updateType = TelegramAdapter


data TelegramUpdate any
    = Ev (Event (TelegramAdapter any))
    | Ignored
    | Unhandeled


data ChatType
    = PrivateChat
    | GroupChat
    | SupergroupChat
    | ChannelChat


declareFields [d|
    data TelegramUser = TelegramUser
        { telegramUserId_ :: Integer
        , telegramUserFirstName :: L.Text
        , telegramUserLastName :: Maybe L.Text
        , telegramUserUsername :: Maybe L.Text
        }
    |]

declareFields [d|

    data TelegramChat = TelegramChat
        { telegramChatId_ :: Integer
        , telegramChatType_ :: ChatType
        , telegramChatUsername :: Maybe L.Text
        , telegramChatFirstName :: Maybe L.Text
        , telegramChatLastName :: Maybe L.Text
        }
    |]

instance FromJSON ChatType where
    parseJSON = withText "expected string" fromStr
      where
        fromStr "private" = pure PrivateChat
        fromStr "group" = pure GroupChat
        fromStr "supergroup" = pure SupergroupChat
        fromStr "channel" = pure ChannelChat
        fromStr _ = mzero

instance FromJSON TelegramUser where
    parseJSON = withObject "user must be object" $ \o ->
        TelegramUser
            <$> o .: "id"
            <*> o .: "first_name"
            <*> o .:? "last_name"
            <*> o .:? "username"

instance FromJSON TelegramChat where
    parseJSON = withObject "channel must be object" $ \o ->
        TelegramChat
            <$> o .: "id"
            <*> o .: "type"
            <*> o .:? "username"
            <*> o .:? "first_name"
            <*> o .:? "last_name"

instance FromJSON (TelegramUpdate any) where
    parseJSON = withObject "expected object" inner
      where
        inner o = isMessage <|> isPost <|> isUnhandeled
          where
            isMessage = do
                msg <- o .: "message" >>= msgParser
                return $ Ev msg
            isPost = Ev <$> (o .: "channel_post" >>= msgParser)
            isUnhandeled = return Unhandeled


msgParser ::  Value -> Parser (Event (TelegramAdapter a))
msgParser = withObject "expected message object" $ \o ->
    MessageEvent
        <$> o .: "from"
        <*> o .: "chat"
        <*> o .: "text"
        <*> (o .: "date" >>= timestampFromNumber)



apiResponseParser :: (Value -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser innerParser = withObject "expected object" $ \o -> do
    ok <- o .: "ok"
    if ok
        then Success <$> o .:? "description" <*> (o .: "result" >>= innerParser)
        else Error <$> o .: "error_code" <*> o .: "description"


execAPIMethod :: MkTelegram b => (Value -> Parser a) -> String -> [FormParam] -> AdapterM (TelegramAdapter b) (Either String (APIResponse a))
execAPIMethod innerParser methodName params = do
    token <- requireFromAdapterConfig "token"
    response <- liftIO $ post $(isS "https://api.telegram.org/bot#{token :: String}/#{methodName}") params
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)


getUsernameImpl :: TelegramUser -> AdapterM (TelegramAdapter a) L.Text
getUsernameImpl u = return $ fromMaybe (u^.firstName) $ u^.username


getChannelNameImpl :: TelegramChat -> AdapterM (TelegramAdapter a) L.Text
getChannelNameImpl c = return $ fromMaybe "<unnamed>" $
    c^.username <|> (L.unwords <$> sequence [c^.firstName, c^.lastName]) <|> c^.firstName


messageChannelImpl :: MkTelegram a => TelegramChat -> L.Text -> AdapterM (TelegramAdapter a) ()
messageChannelImpl chat msg = do
    res <- execAPIMethod msgParser "sendMessage" ["chat_id" := (chat^.id_) , "text" := msg]
    case res of
        Left err -> error $(isS "Unparseable JSON #{err}")
        Right Success{} -> return ()
        Right (Error code desc) ->
            logErrorN $(isT "Sending message failed with #{code}: #{desc}")



runnerImpl :: forall a. MkTelegram a => RunWithAdapter (TelegramAdapter a)
runnerImpl handler = do
    msgChan <- newChan
    let eventGetter = mkEventGetter msgChan
    async eventGetter

    forever $ do
        d <- readChan msgChan
        case d of
            Ev ev -> liftIO $ handler ev
            Ignored -> return ()
            Unhandeled -> logDebugN $(isT "Unhadeled event.")



pollEventGetter :: Chan (TelegramUpdate Poll) -> AdapterM (TelegramAdapter Poll) ()
pollEventGetter msgChan =
    forever $ do
        response <- execAPIMethod parseJSON "getUpdates" []
        case response of
            Left err -> do
                logErrorN $(isT "Unable to parse json: #{err}")
                threadDelay 30000
            Right (Error code desc) -> do
                logErrorN $(isT "Sending message failed with #{code}: #{desc}")
                threadDelay 30000
            Right Success {result=updates} ->
                writeList2Chan msgChan updates


pushEventGetter :: Chan (TelegramUpdate Push) -> AdapterM (TelegramAdapter Push) ()
pushEventGetter msgChan =
    -- port <- liftIO $ C.require cfg "port"
    -- url <- liftIO $ C.require cfg "url"
    return ()



scriptIdImpl :: forall a. MkTelegram a => TelegramAdapter a -> AdapterId (TelegramAdapter a)
scriptIdImpl _ = mkAdapterId (error "phantom value" :: a)


class MkTelegram a where
    mkEventGetter :: Chan (TelegramUpdate a) -> AdapterM (TelegramAdapter a) ()
    mkAdapterId :: a -> AdapterId (TelegramAdapter a)


instance MkTelegram a => IsAdapter (TelegramAdapter a) where
    type User (TelegramAdapter a) = TelegramUser
    type Channel (TelegramAdapter a) = TelegramChat
    adapterId = scriptIdImpl (error "phantom value" :: TelegramAdapter a)
    initAdapter = return TelegramAdapter
    runWithAdapter = runnerImpl
    getUsername = getUsernameImpl
    getChannelName = getChannelNameImpl
    resolveChannel _ = do
        logErrorN "Channel resolving not supported"
        return Nothing
    resolveUser _ = do
        logErrorN "User resolving not supported"
        return Nothing
    messageChannel = messageChannelImpl


data Poll


instance MkTelegram Poll where
    mkAdapterId _ = "telegram-poll"
    mkEventGetter = pollEventGetter


data Push


instance MkTelegram Push where
    mkAdapterId _ = "telegram-push"
    mkEventGetter = pushEventGetter
