{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Marvin.Adapter.Slack.Internal.Types where

import           Control.Concurrent.Chan.Lifted (Chan)
import           Control.Concurrent.MVar.Lifted (MVar)
import           Data.Aeson                     hiding (Error)
import           Data.Aeson.TH
import           Data.Aeson.Types               hiding (Error)
import           Data.Bool                      (bool)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.Foldable                  (toList)
import           Data.Hashable
import           Data.HashMap.Strict            (HashMap)
import           Data.String                    (IsString(..))
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as L
import           Lens.Micro.Platform            hiding ((.=))
import           Marvin.Adapter
import           Marvin.Types
import           Network.URI
import           Util


jsonParseURI :: Value -> Parser URI
jsonParseURI = withText "expected text"
    $ maybe (fail "string not parseable as uri") return . parseURI . T.unpack


data RTMData = RTMData
    { ok  :: Bool
    , url :: URI
    }

type APIResponse a = Either String a


-- | Identifier for a user (internal and not equal to the username)
newtype SlackUserId = SlackUserId { unwrapSlackUserId :: T.Text }
    deriving (IsString, Eq, Hashable)
-- | Identifier for a channel (internal and not equal to the channel name)
newtype SlackChannelId = SlackChannelId { unwrapSlackChannelId :: T.Text }
    deriving (IsString, Eq, Show, Hashable)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''SlackUserId
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''SlackChannelId

class HasTopic s a | s -> a where topic :: Lens' s a
class HasIdValue s a | s -> a where idValue :: Lens' s a
class HasNameResolver s a | s -> a where nameResolver :: Lens' s a
class HasInfoCache s a | s -> a where infoCache :: Lens' s a
class HasCreated s a | s -> a where created :: Lens' s a
class HasChanType s a | s -> a where chanType :: Lens' s a
-- class HasMemberCount s a | s -> a where memberCount :: Lens' s a

data ChannelType
    = PublicChannel
    | PrivateChannel
    | IMChannel
    deriving Show

data LimitedChannelInfo = LimitedChannelInfo
    { limitedChannelInfoIdValue     :: SlackChannelId
    , limitedChannelInfoName        :: Maybe L.Text
    , limitedChannelInfoTopic       :: Maybe L.Text
    -- , limitedChannelInfoChanType    :: ChannelType
    -- , limitedChannelInfoMemberCount :: Int
    } deriving Show

data UserInfo = UserInfo
    { userInfoUsername  :: L.Text
    , userInfoIdValue   :: SlackUserId
    , userInfoName      :: Maybe L.Text
    , userInfoFirstName :: Maybe L.Text
    , userInfoLastName  :: Maybe L.Text
    }

data ChannelCache = ChannelCache
    { channelCacheInfoCache    :: HashMap SlackChannelId LimitedChannelInfo
    , channelCacheNameResolver :: HashMap L.Text LimitedChannelInfo
    }

data UserCache = UserCache
    { userCacheInfoCache    :: HashMap SlackUserId UserInfo
    , userCacheNameResolver :: HashMap L.Text UserInfo
    }

-- | Adapter for interacting with Slack API's. Polymorphic over the method for retrieving events.
data SlackAdapter a = SlackAdapter
    { slackAdapterChannelCache  :: MVar ChannelCache
    , slackAdapterUserInfoCache :: MVar UserCache
    , slackAdapterOutChannel    :: Chan (SlackChannelId, L.Text)
    }


data InternalType a
    = SlackEvent SlackUserId SlackChannelId (UserInfo -> LimitedChannelInfo -> Event (SlackAdapter a))
    | Error
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
    | OkResponseEvent T.Text


data SlackRemoteFile a = SlackRemoteFile
    { slackRemoteFileIdValue         :: L.Text
    , slackRemoteFileCreationDate    :: TimeStamp (SlackAdapter a)
    , slackRemoteFileName            :: Maybe L.Text
    , slackRemoteFileTitle           :: Maybe L.Text
    , slackRemoteFileFileType        :: Maybe L.Text
    , slackRemoteFilePublicPermalink :: Maybe L.Text
    , slackRemoteFileSize            :: Integer
    , slackRemoteFileEditable        :: Bool
    , slackRemoteFilePublic          :: Bool
    , slackRemoteFileUser            :: SlackUserId
    , slackRemoteFileUrl             :: Maybe L.Text
    , slackRemoteFilePrivateUrl      :: L.Text
    }

data SlackLocalFile = SlackLocalFile
    { slackLocalFileName     :: L.Text
    , slackLocalFileFileType :: Maybe L.Text
    , slackLocalFileTitle    :: Maybe L.Text
    , slackLocalFileComment  :: Maybe L.Text
    , slackLocalFileContent  :: FileContent
    }

makeFields ''LimitedChannelInfo
makeFields ''UserInfo
makeFields ''ChannelCache
makeFields ''UserCache
makeFields ''SlackAdapter
makeFields ''SlackRemoteFile
makeFields ''SlackLocalFile

instance FromJSON (TimeStamp (SlackAdapter a)) where parseJSON = timestampFromNumber

instance FromJSON (SlackRemoteFile a) where
    parseJSON = withObject "file must be object" $ \o -> SlackRemoteFile
        <$> o .: "id"
        <*> o .: "created"
        <*> o .:? "name"
        <*> o .:? "title"
        <*> o .:? "filetype"
        <*> o .:? "permalink_public"
        <*> o .: "size"
        <*> o .: "editable"
        <*> o .: "is_public"
        <*> o .: "user"
        <*> o .:? "permalink_public"
        <*> o .: "url_private_download"

-- makeFields ''SlackRemoteFile
-- makeFields ''SlackLocalFile

instance FromJSON RTMData where
    parseJSON = withObject "expected object" $ \o ->
        RTMData <$> o .: "ok" <*> (o .: "url" >>= jsonParseURI)


rawBS :: BS.ByteString -> String
rawBS bs = "\"" ++ BS.unpack bs ++ "\""


helloParser :: Value -> Parser Bool
helloParser = withObject "expected object" $ \o -> do
    t <- o .: "type"
    return $ (t :: T.Text) == "hello"


userInfoParser :: Value -> Parser UserInfo
userInfoParser = withObject "expected object" $ \o -> do
    o2 <- o .: "profile"
    UserInfo
        <$> o .: "name"
        <*> o .: "id"
        <*> o2 .:? "real_name"
        <*> o2 .:? "first_name"
        <*> o2 .:? "last_name"


userInfoListParser :: Value -> Parser [UserInfo]
userInfoListParser = withArray "expected array" (fmap toList . mapM userInfoParser)


apiResponseParser :: (Object -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser f = withObject "expected object" $ \o ->
    o .: "ok" >>= bool
        (Left <$> o .: "error")
        (Right <$> f o)


lciParser :: Value -> Parser LimitedChannelInfo
lciParser = withObject "expected object" $ \o ->
    LimitedChannelInfo
        <$> o .: "id"
        <*> o .:? "name"
        <*> (o .:? "topic" >>= maybe (return Nothing) (fmap Just . withObject "object" (.: "value")))
        -- <*> parseType o
        -- <*> o .: "num_members"
  -- where
  --   parseType o =
  --       ifM (o .: "is_channel")
  --           (pure PublicChannel)
  --           $ ifM (o .: "is_group")
  --               (pure PrivateChannel)
  --               $ ifM (o .: "is_im")
  --                   (pure IMChannel)
  --                   (fail "Expected one of: [is_channel, is_group, is_im] to be true.")


ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond then_ else_ = cond >>= \c -> if c then then_ else else_


lciListParser :: Value -> Parser [LimitedChannelInfo]
lciListParser = withArray "array" $ fmap toList . mapM lciParser
