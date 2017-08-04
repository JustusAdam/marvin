module Marvin.Adapter.Telegram.Internal.Common where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson                      hiding (Error, Success)
import           Data.Aeson.Types                (Parser, parseEither)
import qualified Data.ByteString.Lazy            as BS
import           Data.Char                       (isSpace)
import           Data.Foldable                   (asum)
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Data.Traversable                (for)
import           Lens.Micro.Platform
import           Marvin.Adapter                  hiding (mkAdapterId)
import           Marvin.Interpolate.All
import           Marvin.Types
import           Network.Wreq
import           Network.Wreq.Types
import           Util

data APIResponse a
    = Success { description :: Maybe T.Text, result :: a }
    | Error { errorCode :: Int, errDescription :: T.Text }


-- | The telegram adapter type for a particular update type. Either 'Push' or 'Poll'
data TelegramAdapter updateType = TelegramAdapter


newtype TelegramFileId = TelegramFileId { unwrapFileId :: T.Text } deriving Eq


instance FromJSON TelegramFileId where parseJSON = withText "expected string" (pure . TelegramFileId)
instance ToJSON TelegramFileId where toJSON = String . unwrapFileId

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
data TelegramUser = TelegramUser
    { telegramUserId_       :: Integer
    , telegramUserFirstName :: Maybe L.Text
    , telegramUserLastName  :: Maybe L.Text
    , telegramUserUsername  :: L.Text
    }

data TelegramRemoteFileStruct
    = RemoteAudio
        { telegramRemoteFileStructDuration  :: Integer
        , telegramRemoteFileStructPerformer :: Maybe L.Text
        , telegramRemoteFileStructTitle     :: Maybe L.Text
        }
    | RemoteDocument
    | RemoteSticker
        { telegramRemoteFileStructWidth  :: Integer
        , telegramRemoteFileStructHeight :: Integer
        , telegramRemoteFileStructEmoji  :: Maybe L.Text
        }
    | RemoteVideo
        { telegramRemoteFileStructWidth    :: Integer
        , telegramRemoteFileStructHeight   :: Integer
        , telegramRemoteFileStructDuration :: Integer
        }
    | RemoteVoice
        { telegramRemoteFileStructDuration :: Integer
        }


data TelegramRemoteFile a = TelegramRemoteFile
    { telegramRemoteFileStruct   :: TelegramRemoteFileStruct
    , telegramRemoteFileSize     :: Integer
    , telegramRemoteFileFid      :: TelegramFileId
    , telegramRemoteFileFileType :: Maybe L.Text
    , telegramRemoteFileName     :: Maybe L.Text
    }

data TelegramLocalFileStruct
    = LocalPhoto
    | LocalAudio
        { telegramLocalFileStructDuration  :: Maybe Int
        , telegramLocalFileStructPerformer :: Maybe L.Text
        -- , telegramLocalFileStructShowTitle :: Maybe L.Text
        }
    | LocalDocument
    | LocalVideo
        { telegramLocalFileStructDuration :: Maybe Int
        , telegramLocalFileStructWidth    :: Maybe Int
        , telegramLocalFileStructHeight   :: Maybe Int
        }


data TelegramLocalFile = TelegramLocalFile
    { telegramLocalFileStruct              :: TelegramLocalFileStruct
    , telegramLocalFileContent             :: FileContent
    , telegramLocalFileName                :: L.Text
    , telegramLocalFileFileType            :: Maybe L.Text
    , telegramLocalFileDisableNotification :: Maybe Bool
    , telegramLocalFileFromRef             :: Maybe TelegramFileId
    , telegramLocalFileCaption             :: Maybe L.Text
    }

makeFields ''TelegramUser
makeFields ''TelegramRemoteFileStruct
makeFields ''TelegramRemoteFile
makeFields ''TelegramLocalFileStruct
makeFields ''TelegramLocalFile

instance HasUrl (TelegramRemoteFile a) (Maybe L.Text) where url = lens (const Nothing) const
instance HasCreationDate (TelegramRemoteFile a) (TimeStamp a) where creationDate = lens (const undefined) const


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
data TelegramChat = TelegramChat
    { telegramChatId_       :: Integer
    , telegramChatType_     :: ChatType
    , telegramChatUsername  :: Maybe L.Text
    , telegramChatFirstName :: Maybe L.Text
    , telegramChatLastName  :: Maybe L.Text
    }
makeFields ''TelegramChat

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

parseFileHelper :: (Object -> Parser TelegramRemoteFileStruct) -> Value -> Parser (Object, TelegramRemoteFileStruct)
parseFileHelper f = withObject "expected object" $ \o -> (o, ) <$> f o

parseTelegramAudio :: Object -> Parser TelegramRemoteFileStruct
parseTelegramAudio o =
    RemoteAudio
        <$> o .: "duration"
        <*> o .:? "performer"
        <*> o .:? "title"

parseTelegramDocument :: Object -> Parser TelegramRemoteFileStruct
parseTelegramDocument = const (return RemoteDocument)

parseTelegramSticker :: Object -> Parser TelegramRemoteFileStruct
parseTelegramSticker o =
    RemoteSticker
        <$> o .: "width"
        <*> o .: "height"
        <*> o .:? "emoji"

parseTelegramVideo :: Object -> Parser TelegramRemoteFileStruct
parseTelegramVideo o =
    RemoteVideo
        <$> o .: "width"
        <*> o .: "height"
        <*> o .: "duration"

parseTelegramVoice :: Object -> Parser TelegramRemoteFileStruct
parseTelegramVoice o = RemoteVoice <$> o .: "duration"


parseTelegramFile :: Object -> Parser (TelegramRemoteFile a)
parseTelegramFile o = do
    (o', struct) <- msum
        [ o .: "audio" >>= parseFileHelper parseTelegramAudio
        , o .: "document" >>= parseFileHelper parseTelegramDocument
        , o .: "sticker" >>= parseFileHelper parseTelegramSticker
        , o .: "video" >>= parseFileHelper parseTelegramVideo
        , o .: "voice" >>= parseFileHelper parseTelegramVoice
        ]
    TelegramRemoteFile struct
        <$> o' .:? "file_size" .!= (-1)
        <*> o' .:  "file_id"
        <*> o' .:  "file_name"
        <*> o' .:? "mime_type"


instance MkTelegram a => FromJSON (TelegramUpdate a) where
    parseJSON = withObject "expected object" inner
      where
        inner o = msum [isMessage, isPost, isDocument, isUnhandeled]
          where
            isMessage = Ev <$> (o .: "message" >>= msgParser)
            isPost = Ev <$> (o .: "channel_post" >>= msgParser)
            isUnhandeled = return Unhandeled
            isDocument = do
                file <- parseTelegramFile o
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


msgParser :: Value -> Parser (Event (TelegramAdapter a))
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


execAPIMethod :: (MkTelegram b, Postable p)
              => (Value -> Parser a)
              -> String
              -> p
              -> AdapterM (TelegramAdapter b) (Either String (APIResponse a))
execAPIMethod = execAPIMethodWith defaults

execAPIMethodWith :: (MkTelegram b, Postable p)
                  => Options
                  -> (Value -> Parser a)
                  -> String
                  -> p
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

runnerImpl :: MkTelegram a => EventConsumer (TelegramAdapter a) -> AdapterM (TelegramAdapter a) ()
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


toFtype :: TelegramLocalFileStruct -> (String, T.Text)
toFtype LocalPhoto    = ("Photo", "photo")
toFtype LocalAudio{}  = ("Audio", "audio")
toFtype LocalDocument = ("Document", "document")
toFtype LocalVideo{}  = ("Video", "video")


partShow :: Show a => T.Text -> a -> Part
partShow name = partString name . show

mkStructParts :: TelegramLocalFile -> [Maybe Part]
mkStructParts f =
    case f^.struct of
        s@(LocalAudio duration performer) ->
            [ partText "performer" . L.toStrict <$> performer
            , partShow "duration" <$> duration
            , Just ("title" `partText` L.toStrict (f^.name)) -- perhaps make this optional
            ]
        s@(LocalVideo duration width height) ->
            [ partShow "duration" <$> duration
            , partShow "width" <$> width
            , partShow "height" <$> height
            ]
        _ -> []


shareFileImpl :: MkTelegram a => TelegramLocalFile -> [TelegramChat] -> AdapterM (TelegramAdapter a) (Either L.Text (TelegramRemoteFile (TelegramAdapter a)))
shareFileImpl localFile targets = do
    contentPart <- case localFile^.fromRef of
        Just (TelegramFileId fid) -> return $ propName `partText` fid
        Nothing ->
            (propName `partLBS`) <$> case localFile^.content of
                                        FileInMemory bs -> return bs
                                        FileOnDisk path -> liftIO $ BS.readFile path
    fmap (handleRes <=< headRes) $ for targets $ \chan ->
        execAPIMethod parser ("send" ++ ftype) $ catMaybes $
            [ Just $ "chat_id" `partString` show (chan ^. id_)
            , Just contentPart
            , partText "caption" . L.toStrict <$> localFile^.caption
            , partText "diable_notofication" . boolToText <$> localFile^.disableNotification
            ] ++ structParts
  where
    parser = withObject "expected object" parseTelegramFile
    (ftype, propName) = toFtype (localFile^.struct)
    structParts = mkStructParts localFile
    boolToText True = "true"
    boolToText _    = "false"
    headRes []    = Left "No targets specified"
    headRes (x:_) = mapLeft L.pack x
    handleRes Success{result=a}       = pure a
    handleRes Error{errDescription=d} = Left $ L.fromStrict d


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

instance MkTelegram a => SupportsFiles (TelegramAdapter a) where
    type RemoteFile (TelegramAdapter a) = TelegramRemoteFile (TelegramAdapter a)
    type LocalFile (TelegramAdapter a) = TelegramLocalFile
    shareFile = shareFileImpl
    readFileBytes remoteFile = do
        res <- execAPIMethod (withObject "expected object" (.: "file_path")) "getFile" ["file_id" := unwrapFileId (remoteFile^.fid)]
        case res of
            Left err -> logErrorN $(isT "Error when downloading file: #{err}") >> return Nothing
            Right Error{errDescription=desc} -> logErrorN $(isT "Error when downloading file: #{desc}") >> return Nothing
            Right Success{result=Nothing} -> logErrorN $(isT "Fetched file had no filepath") >> return Nothing
            Right Success{result=Just fp} -> do
                token <- requireFromAdapterConfig "token"
                fres <- liftIO $ get $(is "https://api.telegram.org/bot#{token}/#{fp}")
                case fres ^. responseStatus . statusCode of
                    200 -> return $ Just $ fres ^. responseBody
                    code -> do
                        logErrorN $(isT "Non 200 Response when downloading file: #{code} - #{fres^.responseStatus.statusMessage}")
                        return Nothing
    newLocalFile fname content = pure $ TelegramLocalFile LocalDocument content fname Nothing Nothing Nothing Nothing
