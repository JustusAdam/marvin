module Marvin.Adapter.Telegram.Common where

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
import Control.Lens
import Control.Monad
import Unsafe.Coerce


data Push
data Poll


data APIResponse a
    = Success { description :: Maybe T.Text, result :: a}
    | Error { errorCode :: Int, errDescription :: T.Text}


data InternalEvent any 
    = Ev (Event (TelegramAdapter any))
    | Ignored
    | Unhandeled


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


runnerImpl :: (T.Text -> Chan BL.ByteString -> RunnerM ()) -> RunWithAdapter (TelegramAdapter a)
runnerImpl eventGetter config initializer = do
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





data TelegramAdapter updateType = TelegramAdapter
    { userConfig :: C.Config
    }
