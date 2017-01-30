{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Marvin.Adapter.Telegram.Common where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error, Success)
import           Data.Aeson.Types                (Parser, parseEither)
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter                  hiding (mkAdapterId)
import           Marvin.Interpolate.All
import           Network.Wreq
import           Util


data APIResponse a
    = Success { description :: Maybe T.Text, result :: a}
    | Error { errorCode :: Int, errDescription :: T.Text}


-- | The telegram adapter type for a particular update type. Either 'Push' or 'Poll'
data TelegramAdapter updateType = TelegramAdapter


data TelegramUpdate any
    = Ev (Event (TelegramAdapter any))
    | Ignored
    | Unhandeled


-- | Chat type as defined by the telegram api
data ChatType
    = PrivateChat
    | GroupChat
    | SupergroupChat
    | ChannelChat


-- | A user object as contained in the telegram update objects
declareFields [d|
    data TelegramUser = TelegramUser
        { telegramUserId_       :: Integer
        , telegramUserFirstName :: L.Text
        , telegramUserLastName  :: Maybe L.Text
        , telegramUserUsername  :: Maybe L.Text
        }
    |]

-- | A telegram chat object as contained in telegram updates
declareFields [d|
    data TelegramChat = TelegramChat
        { telegramChatId_       :: Integer
        , telegramChatType_     :: ChatType
        , telegramChatUsername  :: Maybe L.Text
        , telegramChatFirstName :: Maybe L.Text
        , telegramChatLastName  :: Maybe L.Text
        }
    |]

instance FromJSON ChatType where
    parseJSON = withText "expected string" $
        \case
            "private" -> pure PrivateChat
            "group" -> pure GroupChat
            "supergroup" -> pure SupergroupChat
            "channel" -> pure ChannelChat
            a -> fail $(isS "Unknown chat type #{a}")

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


-- | Class to enable polymorphism over update mechanics for 'TelegramAdapter'
class MkTelegram a where
    mkEventGetter :: Chan (TelegramUpdate a) -> AdapterM (TelegramAdapter a) ()
    mkAdapterId :: AdapterId (TelegramAdapter a)


instance MkTelegram a => IsAdapter (TelegramAdapter a) where
    type User (TelegramAdapter a) = TelegramUser
    type Channel (TelegramAdapter a) = TelegramChat
    adapterId = mkAdapterId
    initAdapter = return TelegramAdapter
    runWithAdapter = runnerImpl
    getUsername = getUsernameImpl
    getChannelName = getChannelNameImpl
    -- | Not supported in this adapter and always returns 'Nothing'
    resolveChannel _ = do
        logErrorN "Channel resolving not supported"
        return Nothing
    -- | Not supported in this adapter and always returns 'Nothing'
    resolveUser _ = do
        logErrorN "User resolving not supported"
        return Nothing
    messageChannel = messageChannelImpl
