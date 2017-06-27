{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Char                       (isSpace)
import           Data.Foldable                   (asum)
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Marvin.Adapter                  hiding (mkAdapterId)
import           Marvin.Interpolate.All
import           Marvin.Types
import           Network.Wreq
import           Util

import           Control.Exception.Lifted

data APIResponse a
    = Success { description :: Maybe T.Text, result :: a }
    | Error { errorCode :: Int, errDescription :: T.Text }


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

class HasId_ s a | s -> a where id_ :: Lens' s a

-- | A user object as contained in the telegram update objects
declareFields [d|
    data TelegramUser = TelegramUser
        { telegramUserId_       :: Integer
        , telegramUserFirstName :: Maybe L.Text
        , telegramUserLastName  :: Maybe L.Text
        , telegramUserUsername  :: L.Text
        }
    |]

declareFields [d|
    data TelegramFileStruct
        = TelegramAudio
            { telegramFileStructDuration :: Integer
            , telegramFileStructPerformer :: Maybe L.Text
            , telegramFileStructTitle :: Maybe L.Text
            }
        | TelegramDocument
        | TelegramSticker 
            { telegramFileStructWidth :: Integer
            , telegramFileStructHeight :: Integer
            , telegramFileStructEmoji :: Maybe L.Text
            }
        | TelegramVideo
            { telegramFileStructWidth :: Integer
            , telegramFileStructHeight :: Integer
            , telegramFileStructDuration :: Integer
            }
        | TelegramVoice 
            { telegramFileStructDuration :: Integer
            }
    |]


declareFields [d|
    data TelegramFile a = TelegramFile
        { telegramFileFile :: TelegramFileStruct
        , telegramFileSize :: Integer
        , telegramFileFid  :: T.Text
        , telegramFileName :: Maybe L.Text
        }
    |]

instance HasUrl (TelegramFile a) (Maybe L.Text) where url = lens (const Nothing) const
instance HasCreationDate (TelegramFile a) (TimeStamp a) where creationDate = lens (const undefined) const


instance HasName TelegramUser (Maybe L.Text) where
    name = lens
        (\u -> (mappend <$> u^.firstName <*> (mappend " " <$> u^.lastName)) <|> u^.firstName <|> u^.lastName)
        (flip set_)
      where
        set_ Nothing = (firstName .~ Nothing) . (lastName .~ Nothing)
        set_ (Just n) = (firstName .~ Just first) . (lastName .~ if L.null rest then Nothing else Just rest)
          where
            (first, rest') = L.break (== ' ') n
            rest = L.tail rest'


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

instance HasName TelegramChat L.Text where
    name = lens
        (\c -> fromMaybe "" $ c^.username)
        (\c n -> c&username.~(case n of "" -> Nothing;a->Just a))

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
            <*> (Just <$> o .: "first_name")
            <*> o .:? "last_name"
            <*> (maybe (o .: "first_name") return =<< o .:? "username")

instance FromJSON TelegramChat where
    parseJSON = withObject "channel must be object" $ \o ->
        TelegramChat
            <$> o .: "id"
            <*> o .: "type"
            <*> o .:? "username"
            <*> o .:? "first_name"
            <*> o .:? "last_name"

parseFileHelper :: (Object -> Parser TelegramFileStruct) -> Value -> Parser (Object, TelegramFileStruct)
parseFileHelper f = withObject "expected object" $ \o -> (o, ) <$> f o

parseTelegramAudio :: Object -> Parser TelegramFileStruct
parseTelegramAudio o =
    TelegramAudio
        <$> o .: "duration"
        <*> o .:? "performer"
        <*> o .:? "title"

parseTelegramDocument :: Object -> Parser TelegramFileStruct
parseTelegramDocument = const (return TelegramDocument)

parseTelegramSticker :: Object -> Parser TelegramFileStruct
parseTelegramSticker o =
    TelegramSticker
        <$> o .: "width"
        <*> o .: "height"
        <*> o .:? "emoji"

parseTelegramVideo :: Object -> Parser TelegramFileStruct
parseTelegramVideo o =
    TelegramVideo
        <$> o .: "width"
        <*> o .: "height"
        <*> o .: "duration"

parseTelegramVoice :: Object -> Parser TelegramFileStruct
parseTelegramVoice o = TelegramVoice <$> o .: "duration"


instance FromJSON (TelegramUpdate any) where
    parseJSON = withObject "expected object" inner
      where
        inner o = msum [isMessage, isPost, isDocument, isUnhandeled]
          where
            isMessage = do
                msg <- o .: "message" >>= msgParser
                return $ Ev msg
            isPost = Ev <$> (o .: "channel_post" >>= msgParser)
            isUnhandeled = return Unhandeled
            isDocument = do
                (o', struct) <- msum 
                    [ o .: "audio" >>= parseFileHelper parseTelegramAudio
                    , o .: "document" >>= parseFileHelper parseTelegramDocument
                    , o .: "sticker" >>= parseFileHelper parseTelegramSticker
                    , o .: "video" >>= parseFileHelper parseTelegramVideo
                    , o .: "voice" >>= parseFileHelper parseTelegramVoice
                    ]
                file <- TelegramFile struct 
                    <$> o' .:? "file_size" .!= (-1)
                    <*> o' .: "file_id"
                    <*> o' .: "file_name"
                ev <- FileSharedEvent 
                    <$> o .: "from"
                    <*> o .: "chat"
                    <*> pure file
                    <*> (o .: "date" >>= timestampFromNumber)
                pure $ Ev ev



telegramSupportedUpdates :: [T.Text]
telegramSupportedUpdates =
    [ "message"
    , "channel_post"
    ]


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


execAPIMethod :: MkTelegram b
              => (Value -> Parser a)
              -> String
              -> [FormParam]
              -> AdapterM (TelegramAdapter b) (Either String (APIResponse a))
execAPIMethod = execAPIMethodWith defaults

execAPIMethodWith :: MkTelegram b
                  => Options
                  -> (Value -> Parser a)
                  -> String
                  -> [FormParam]
                  -> AdapterM (TelegramAdapter b) (Either String (APIResponse a))
execAPIMethodWith opts innerParser methodName fparams = do
    token <- requireFromAdapterConfig "token"
    res <- retry (3 :: Int) (liftIO (postWith opts $(isS "https://api.telegram.org/bot#{token :: String}/#{methodName}") fparams))
    return $ res >>= eitherDecode . (^. responseBody) >>= parseEither (apiResponseParser innerParser)
  where
    retry n a = (Right <$> a) `catch` \e -> if n <= 0
                                                then -- TODO only catch appropriate exceptions
                                                    return $ Left $ displayException (e :: SomeException)
                                                else retry (succ n) a

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

stripWhiteSpaceMay :: L.Text -> Maybe L.Text
stripWhiteSpaceMay t =
    case L.uncons t of
        Just (c, _) | isSpace c -> Just $ L.stripStart t
        _           -> Nothing

runnerImpl :: forall a. MkTelegram a => EventConsumer (TelegramAdapter a) -> AdapterM (TelegramAdapter a) ()
runnerImpl handler = do
    msgChan <- newChan
    let eventGetter = mkEventGetter msgChan
    a <- async eventGetter
    link a

    forever $ do
        logDebugN "Starting to read"
        d <- readChan msgChan
        logDebugN "Recieved message"
        case d of
            Ev ev@(MessageEvent u chat msg ts) -> do
                botname <- L.toLower <$> getBotname
                let strippedMsg = L.stripStart msg
                let lmsg = L.toLower strippedMsg
                handler $ case (chat^.type_, asum $ map ((\prefix -> if prefix `L.isPrefixOf` lmsg then Just $ L.drop (L.length prefix) strippedMsg else Nothing) >=> stripWhiteSpaceMay) [botname, L.cons '@' botname, L.cons '/' botname]) of
                    (PrivateChat, _) -> CommandEvent u chat msg ts
                    (_, Nothing)     -> ev
                    (_, Just m')     -> CommandEvent u chat m' ts
            Ev ev -> handler ev
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
    runAdapter = runnerImpl
    resolveChannel _ = do
        logErrorN "Channel resolving not supported"
        return Nothing
    resolveUser _ = do
        logErrorN "User resolving not supported"
        return Nothing
    messageChannel = messageChannelImpl

instance HasFiles (TelegramAdapter a) where
    type RemoteFile (TelegramAdapter a) = TelegramFile (TelegramAdapter a)
