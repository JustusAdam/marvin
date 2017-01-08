{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Marvin.Adapter.Telegram
    ( TelegramAdapter, Push, Poll
    , TelegramChat(..), ChatType(..)
    , TelegramUser(..)
    , HasId_(id_), HasUsername(username), HasFirstName(firstName), HasLastName(lastName), HasType_(type_)
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
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import qualified Data.Configurator               as C
import qualified Data.Configurator.Types         as C
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Text.Lazy                  as L
import qualified Data.Text.Lazy.Encoding         as L
import           Marvin.Adapter
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text
import           Marvin.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wreq


data APIResponse a
    = Success { description :: Maybe T.Text, result :: a}
    | Error { errorCode :: Int, errDescription :: T.Text}


data TelegramAdapter updateType = TelegramAdapter
    { userConfig :: C.Config
    }


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
        inner o = isMessage <|> isUnhandeled
          where
            isMessage = do
                msg <- o .: "message" >>= msgParser
                return $ Ev $ MessageEvent msg
            isPost = Ev . MessageEvent <$> (o .: "channel_post" >>= msgParser)
            isUnhandeled = return Unhandeled


msgParser ::  Value -> Parser (Message (TelegramAdapter a))
msgParser = withObject "expected message object" $ \o ->
    Message
        <$> o .: "from"
        <*> o .: "chat"
        <*> o .: "text"
        <*> o .: "date"



apiResponseParser :: (Value -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser innerParser = withObject "expected object" $ \o -> do
    ok <- o .: "ok"
    if ok
        then Success <$> o .:? "description" <*> (o .: "result" >>= innerParser)
        else Error <$> o .: "error_code" <*> o .: "description"


execAPIMethod :: (Value -> Parser a) -> TelegramAdapter b -> String -> [FormParam] -> RunnerM (Either String (APIResponse a))
execAPIMethod innerParser adapter methodName params = do
    token <- liftIO $ C.require cfg "token"
    response <- liftIO $ post $(isS "https://api.telegram.org/bot#{token :: String}/#{methodName}") params
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)
  where
    cfg = userConfig adapter


getUsernameImpl :: TelegramAdapter a -> TelegramUser -> RunnerM L.Text
getUsernameImpl _ u = return $ fromMaybe (u^.firstName) $ u^.username


getChannelNameImpl :: TelegramAdapter a -> TelegramChat -> RunnerM L.Text
getChannelNameImpl _ c = return $ fromMaybe "<unnamed>" $
    c^.username <|> (L.unwords <$> sequence [c^.firstName, c^.lastName]) <|> c^.firstName


messageChannelImpl :: TelegramAdapter a -> TelegramChat -> L.Text -> RunnerM ()
messageChannelImpl ada chat msg = do
    res <- execAPIMethod msgParser ada "sendMessage" ["text" := msg]
    case res of
        Left err -> error $(isS "Unparseable JSON #{err}")
        Right Success{} -> return ()
        Right (Error code desc) ->
            logErrorN $(isT "Sending message failed with #{code}: #{desc}")



runnerImpl :: forall a. MkTelegram a => RunWithAdapter (TelegramAdapter a)
runnerImpl config initializer = do
    loggingFn <- askLoggerIO
    msgChan <- newChan
    let ada = TelegramAdapter config
    handler <- liftIO $ initializer ada
    let eventGetter = mkEventGetter ada msgChan
    async eventGetter

    forever $ do
        d <- readChan msgChan
        case d of
            Ev ev -> liftIO $ handler ev
            Ignored -> return ()
            Unhandeled -> logDebugN $(isT "Unhadeled event.")



pollEventGetter :: TelegramAdapter Poll -> Chan (TelegramUpdate Poll) -> RunnerM ()
pollEventGetter ada msgChan =
    forever $ do
        response <- execAPIMethod parseJSON ada "getUpdates" []
        case response of
            Left err -> do
                logErrorN $(isT "Unable to parse json: #{err}")
                threadDelay 30000
            Right (Error code desc) -> do
                logErrorN $(isT "Sending message failed with #{code}: #{desc}")
                threadDelay 30000
            Right Success {result=updates} ->
                writeList2Chan msgChan updates


pushEventGetter :: TelegramAdapter Push -> Chan (TelegramUpdate Push) -> RunnerM ()
pushEventGetter ada msgChan =
    -- port <- liftIO $ C.require cfg "port"
    -- url <- liftIO $ C.require cfg "url"
    return ()
  where
    cfg = userConfig ada



scriptIdImpl :: forall a. MkTelegram a => TelegramAdapter a -> AdapterId (TelegramAdapter a)
scriptIdImpl _ = mkAdapterId (error "phantom value" :: a)


class MkTelegram a where
    mkEventGetter :: TelegramAdapter a -> Chan (TelegramUpdate a) -> RunnerM ()
    mkAdapterId :: a -> AdapterId (TelegramAdapter a)


instance MkTelegram a => IsAdapter (TelegramAdapter a) where
    type User (TelegramAdapter a) = TelegramUser
    type Channel (TelegramAdapter a) = TelegramChat
    adapterId = scriptIdImpl (error "phantom value" :: TelegramAdapter a)
    runWithAdapter = runnerImpl
    getUsername = getUsernameImpl
    getChannelName = getChannelNameImpl
    resolveChannel _ _ = do
        logErrorN "Channel resolving not supported"
        return Nothing
    messageChannel = messageChannelImpl


data Poll


instance MkTelegram Poll where
    mkAdapterId _ = "telegram-poll"
    mkEventGetter = pollEventGetter


data Push


instance MkTelegram Push where
    mkAdapterId _ = "telegram-push"
    mkEventGetter _ = error "not implemented"
