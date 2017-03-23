{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Marvin.Adapter.Slack.Types where

import           Control.Concurrent.Chan.Lifted (Chan)
import           Control.Concurrent.MVar.Lifted (MVar)
import           Control.Lens                   hiding ((.=))
import           Data.Aeson                     hiding (Error)
import           Data.Aeson.TH
import           Data.Aeson.Types               hiding (Error)
import qualified Data.ByteString.Lazy.Char8     as BS
import           Data.Foldable                  (toList)
import           Data.Hashable
import           Data.HashMap.Strict            (HashMap)
import           Data.List                      (stripPrefix)
import           Data.Maybe                     (fromJust)
import           Data.String                    (IsString (..))
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as L
import           Marvin.Adapter
import           Marvin.Types
import           Network.URI
import           Util


jsonParseURI :: Value -> Parser URI
jsonParseURI =  withText "expected text" $ maybe (fail "string not parseable as uri") return . parseURI . T.unpack


data RTMData = RTMData
    { ok  :: Bool
    , url :: URI
    }

type APIResponse a = Either String a


-- | Identifier for a user (internal and not equal to the username)
newtype SlackUserId = SlackUserId T.Text deriving (IsString, Eq, Hashable)
-- | Identifier for a channel (internal and not equal to the channel name)
newtype SlackChannelId = SlackChannelId T.Text deriving (IsString, Eq, Show, Hashable)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''SlackUserId
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''SlackChannelId

class HasTopic s a | s -> a where topic :: Lens' s a
class HasIdValue s a | s -> a where idValue :: Lens' s a
class HasNameResolver s a | s -> a where nameResolver :: Lens' s a
class HasInfoCache s a | s -> a where infoCache :: Lens' s a
class HasCreated s a | s -> a where created :: Lens' s a

declareFields [d|
    data LimitedChannelInfo = LimitedChannelInfo
        { limitedChannelInfoIdValue :: SlackChannelId
        , limitedChannelInfoName    :: L.Text
        , limitedChannelInfoTopic   :: L.Text
        } deriving Show
    |]

declareFields [d|
    data UserInfo = UserInfo
        { userInfoUsername  :: L.Text
        , userInfoIdValue   :: SlackUserId
        , userInfoName      :: Maybe L.Text
        , userInfoFirstName :: Maybe L.Text
        , userInfoLastName  :: Maybe L.Text
        }
    |]


declareFields [d|
    data ChannelCache = ChannelCache
        { channelCacheInfoCache    :: HashMap SlackChannelId LimitedChannelInfo
        , channelCacheNameResolver :: HashMap L.Text LimitedChannelInfo
        }
    |]


declareFields [d|
    data UserCache = UserCache
        { userCacheInfoCache    :: HashMap SlackUserId UserInfo
        , userCacheNameResolver :: HashMap L.Text UserInfo
        }
    |]

data InternalType a
    = SlackEvent (SlackUserId, SlackChannelId, UserInfo -> LimitedChannelInfo -> Event (SlackAdapter a))
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


-- | Adapter for interacting with Slack API\'s. Polymorphic over the method for retrieving events.
data SlackAdapter a = SlackAdapter
    { channelCache  :: MVar ChannelCache
    , userInfoCache :: MVar UserCache
    , outChannel    :: Chan (SlackChannelId, L.Text)
    }

instance FromJSON (TimeStamp (SlackAdapter a)) where parseJSON = timestampFromNumber

declareFields [d|
    data SlackRemoteFile a = SlackRemoteFile
        { slackRemoteFileIdValue        :: L.Text
        , slackRemoteFileCreationDate   :: TimeStamp (SlackAdapter a)
        , slackRemoteFileName           :: Maybe L.Text
        , slackRemoteFileTitle          :: Maybe L.Text
        , slackRemoteFileType_          :: Maybe L.Text
        , slackRemoteFileUrl            :: Maybe L.Text
        , slackRemoteFileSize           :: Int
        , slackRemoteFileEditable       :: Bool
        , slackRemoteFilePublic         :: Bool
        , slackRemoteFileUser           :: SlackUserId
        }
    |]


data SlackLocalFile = SlackLocalFile
    { slackLocalFileName :: Maybe L.Text
    , slackLocalFileType_ :: Maybe L.Text
    , slackLocalFileContent :: FileContent
    }

instance FromJSON (SlackRemoteFile a) where
    parseJSON = withObject "file must be object" $ \o -> SlackRemoteFile 
        <$> o .: "id"
        <*> o .: "created"
        <*> o .:? "name"
        <*> o .:? "title"
        <*> o .:? "filetype"
        <*> o .:? "permalink"
        <*> o .: "size"
        <*> o .: "editable"
        <*> o .: "public"
        <*> o .: "user"

makeFields ''SlackRemoteFile
makeFields ''SlackLocalFile

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
userInfoParser = withObject "expected object" $ \o ->
    o .: "user" >>= 
        withObject "expected object" (\o' -> do
            o2 <- o' .: "profile"
            UserInfo <$> o' .: "name" <*> o' .: "id"
                <*> o2 .:? "real_name"
                <*> o2 .:? "first_name"
                <*> o2 .:? "last_name"
                
        )


userInfoListParser :: Value -> Parser [UserInfo]
userInfoListParser = withArray "expected array" (fmap toList . mapM userInfoParser)


apiResponseParser :: (Object -> Parser a) -> Value -> Parser (APIResponse a)
apiResponseParser f = withObject "expected object" $ \o -> do
    succ <- o .: "ok"
    if succ
        then Right <$> f o
        else Left <$> o .: "error"


lciParser :: Value -> Parser LimitedChannelInfo
lciParser = withObject "expected object" $ \o ->
    LimitedChannelInfo
        <$> o .: "id"
        <*> o .: "name"
        <*> (o .: "topic" >>= withObject "object" (.: "value"))

lciListParser :: Value -> Parser [LimitedChannelInfo]
lciListParser = withArray "array" $ fmap toList . mapM lciParser

