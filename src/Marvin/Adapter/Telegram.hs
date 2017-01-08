{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, FunctionalDependencies, FlexibleInstances #-}
module Marvin.Adapter.Telegram where

import Marvin.Adapter
import Network.Wreq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import Data.Aeson hiding (Error, Success)
import Data.Aeson.Types (Parser, parseEither)
import Control.Concurrent.Chan.Lifted
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Control.Concurrent.Async.Lifted
import Control.Monad.Logger
import Marvin.Interpolate.Text
import Marvin.Interpolate.String
import Control.Applicative
import Marvin.Types
import Control.Monad.IO.Class
import Control.Monad
import Unsafe.Coerce
import Control.Lens


data APIResponse a
    = Success { description :: Maybe T.Text, result :: a}
    | Error { errorCode :: Int, errDescription :: T.Text}


data TelegramAdapter updateType = TelegramAdapter
    { userConfig :: C.Config
    }


data InternalEvent any 
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
        , telegramUserUserName :: Maybe L.Text
        }

    data TelegramChannel = TelegramChannel
        { telegramChatId_ :: Integer
        , telegramChatType_ :: ChatType
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

instance FromJSON TelegramChannel where
    parseJSON = withObject "channel must be object" $ \o ->
        TelegramChannel
            <$> o .: "id"
            <*> o .: "type"

instance FromJSON (InternalEvent any) where
    parseJSON = withObject "expected object" inner
      where
        inner o = isMessage <|> isUnhandeled
          where
            isMessage = do 
                msg <- o .: "message" >>= msgParser
                return $ Ev $ MessageEvent msg
            isPost = Ev . MessageEvent <$> (o .: "channel_post" >>= msgParser)
            isUnhandeled = return Unhandeled

        msgParser = withObject "expected message object" $ \o -> 
            Message
                <$> (o .: "from" >>= withObject "expected user object" (.: "id"))
                <*> (o .: "chat" >>= withObject "expected channel object" (.: "id"))
                <*> o .: "text"
                <*> o .: "date" 



apiResponseParser :: (Object -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser innerParser = withObject "expected object" $ \o -> do
    ok <- o .: "ok"
    if ok
        then Success <$> o .:? "description" <*> innerParser o
        else Error <$> o .: "error_code" <*> o .: "description"


execAPIMethod :: (Object -> Parser a) -> TelegramAdapter b -> String -> [FormParam] -> RunnerM (Either String (APIResponse a))
execAPIMethod innerParser adapter methodName params = do
    token <- liftIO $ C.require cfg "token"
    response <- liftIO $ post $(isS "https://api.telegram.org/bot#{token :: String}/#{methodName}") params
    return $ eitherDecode (response^.responseBody) >>= parseEither (apiResponseParser innerParser)
  where
    cfg = userConfig adapter


runnerImpl :: forall a. MkTelegram a => RunWithAdapter (TelegramAdapter a)
runnerImpl config initializer = do
    token <- liftIO $ C.require config "token"
    loggingFn <- askLoggerIO
    msgChan <- newChan
    let ada = TelegramAdapter config
    handler <- liftIO $ initializer ada
    async (eventGetter token msgChan)

    forever $ do
        d <- readChan msgChan
        case eitherDecode d of
            Left err -> logErrorN $(isT "Error parsing json: #{err} original data: #{L.decodeUtf8 d}")
            Right (Ev ev) -> liftIO $ handler ev
            Right event ->
                case event of
                    Ignored -> return ()
                    Unhandeled -> logDebugN $(isT "Unhadeled event. Original data #{ L.decodeUtf8 d}")
  where
    eventGetter = mkEventGetter (error "phantom value" :: a)


scriptIdImpl :: forall a. MkTelegram a => TelegramAdapter a -> AdapterId (TelegramAdapter a)
scriptIdImpl _ = mkAdapterId (error "phantom value" :: a)


class MkTelegram a where
    mkEventGetter :: a -> T.Text -> Chan BL.ByteString -> RunnerM ()
    mkAdapterId :: a -> AdapterId (TelegramAdapter a)


instance MkTelegram a => IsAdapter (TelegramAdapter a) where
    type User (TelegramAdapter a) = TelegramUser
    type Channel (TelegramAdapter a) = TelegramChannel
    adapterId = scriptIdImpl (undefined :: TelegramAdapter a)
    runWithAdapter = runnerImpl


data Poll


instance MkTelegram Poll where
    mkAdapterId _ = "telegram-poll"
    mkEventGetter _ = error "not implemented"


data Push


instance MkTelegram Push where
    mkAdapterId _ = "telegram-push"
    mkEventGetter _ = error "not implemented"
