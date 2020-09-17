{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Internal.Types where

#define HAS_SEMIGROUP MIN_VERSION_base(4,9,0)

import           Control.DeepSeq
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.ByteString.Lazy        (ByteString)
import           Data.Char                   (isAlphaNum, isLetter)
import qualified Data.HashMap.Strict         as HM
import           Data.String
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import qualified Data.Text.Lazy.Encoding     as L
import           Data.Time.Clock
import           Data.Vector                 (Vector)
import           GHC.Generics
import           Labels
import           Lens.Micro.Platform
import           Marvin.Internal.LensClasses
import           Marvin.Interpolate.All
import qualified Marvin.Util.Config          as C
import           Marvin.Util.Regex

#if HAS_SEMIGROUP
import           Data.Semigroup              as S
#else
import           Data.Monoid
#endif

-- | The topic in a channel
type Topic = L.Text
-- | The contents of a recieved message
type Message = L.Text

-- | A timestamp type. Supplied with most 'Event' types.
--
-- The type parameter is only a tag.
-- It is used so you may declare instances for classes such as 'FromJSON' for this type depending on your adapter.
-- This way different adapters may have different 'FromJSON' instances for the timestamp.
newtype TimeStamp a = TimeStamp { toUTCTime :: UTCTime } deriving (Show, Eq, Ord)


data AdapterMEnv a = AdapterMEnv
    { adapterMEnvConfig  :: C.Config
    , adapterMEnvAdapter :: a
    }

makeFields ''AdapterMEnv


-- | Representation for the types of events which can occur
data Event a
    = MessageEvent (User a) (Channel a) Message (TimeStamp a)
    | CommandEvent (User a) (Channel a) Message (TimeStamp a)
    | ChannelJoinEvent (User a) (Channel a) (TimeStamp a)
    | ChannelLeaveEvent (User a) (Channel a) (TimeStamp a)
    | TopicChangeEvent (User a) (Channel a) Topic (TimeStamp a)
    | SupportsFiles a => FileSharedEvent (User a) (Channel a) (RemoteFile a) (TimeStamp a)

-- | Basic monad which most internal actions run in
type RunnerM = LoggingT IO

-- | Handles a single event using a certain adapter
type EventConsumer a = Event a -> AdapterM a ()

-- | Basic functionality required of any adapter
class ( HasUsername (User a) L.Text
      , HasName (User a) (Maybe L.Text)
      , HasFirstName (User a) (Maybe L.Text)
      , HasLastName (User a) (Maybe L.Text)
      , HasName (Channel a) (Maybe L.Text)
      ) => IsAdapter a where
    -- | Concrete, adapter specific representation of a user. Could be an id string or a full object for instance
    type User a
    -- | Concrete, adapter specific representation of a channel. Could be an id string or a full object for instance
    type Channel a
    -- | Used for scoping config and logging
    adapterId :: AdapterId a
    -- | Post a message to a channel given the internal channel identifier
    messageChannel :: Channel a -> L.Text -> AdapterM a ()
    -- | Initialize the adapter state
    initAdapter :: RunnerM a
    -- | Run the recieving side of the adapter.
    -- The 'EventConsumer' argument is a function which should be called once for each recieved 'Event'.
    -- The 'EventConsumer' will trigger the processing of the event by the installed handlers.
    -- However the 'EventConsumer' is deliberately early retuning, meaning when it returns processing of the 'Event' has been queued but not finished yet. In fact it most likely has just started.
    -- It its therefore not necessary to run the 'EventConsumer' in parallel, using 'async' for instance.
    runAdapter :: EventConsumer a -> AdapterM a ()
    -- | Resolve to the internal channel structure given a human readable name
    resolveChannel :: L.Text -> AdapterM a (Maybe (Channel a))
    -- | Resolve to the internal user structure given a human readable name
    resolveUser :: L.Text -> AdapterM a (Maybe (User a))


-- | Monad in which adapter actions run in
newtype AdapterM a r = AdapterM { runAdapterAction :: ReaderT (AdapterMEnv a) RunnerM r }
    deriving (MonadIO, Monad, Applicative, Functor, MonadLogger, MonadLoggerIO, MonadBase IO, MonadReader (AdapterMEnv a))


-- | Content of a file. Can be preloaded in memory or be a persistent file on disc.
data FileContent = FileOnDisk FilePath | FileInMemory ByteString


class ( HasName (RemoteFile a) (Maybe L.Text)
      , HasUrl (RemoteFile a) (Maybe L.Text)
      , HasFileType (RemoteFile a) (Maybe L.Text)
      , HasCreationDate (RemoteFile a) (TimeStamp a)
      , HasSize (RemoteFile a) Integer
      , HasContent (LocalFile a) FileContent
      , HasName (LocalFile a) L.Text
      , HasFileType (LocalFile a) (Maybe L.Text)
      ) => SupportsFiles a where
    -- | Concrete type of an uploaded file
    type RemoteFile a
    type LocalFile a
    -- | Resolve the name of the file
    newLocalFile :: L.Text -> FileContent -> AdapterM a (LocalFile a)
    readTextFile :: RemoteFile a -> AdapterM a (Maybe L.Text)
    readTextFile = fmap (fmap L.decodeUtf8) . readFileBytes
    readFileBytes :: RemoteFile a -> AdapterM a (Maybe ByteString)
    shareFile :: LocalFile a -> [Channel a] -> AdapterM a (Either L.Text (RemoteFile a))
    -- Add a method for resolving a file


-- | Wrapping type for users. Only used to enable 'Get' typeclass instances.
newtype User' a = User' { unwrapUser' :: User a }
-- | Wrapping type for channels. Only used to enable 'Get' typeclass instances.
newtype Channel' a = Channel' { unwrapChannel' :: Channel a }

-- | Wrapping type for files. Only used to enable 'Get' typeclass instances.
newtype RemoteFile' a = File' { unwrapFile' :: RemoteFile a }

-- | A type, basically a String, which identifies a script to the config and the logging facilities.
--
-- For conversion please use 'mkScriptId' and 'unwrapScriptId'. They will perform necessary checks.
newtype ScriptId = ScriptId { unwrapScriptId :: T.Text } deriving (Show, Eq)


-- | A type, basically a String, which identifies an adapter to the config and the logging facilities.
--
-- For conversion please use 'mkAdapterId' and 'unwrapAdapterId'. They will perform necessary checks.
newtype AdapterId a = AdapterId { unwrapAdapterId :: T.Text } deriving (Show, Eq)


-- | Read only data available to a handler when the bot reacts to an event.
data BotActionState a d = BotActionState
    { botActionStateScriptId :: ScriptId
    , botActionStateConfig   :: C.Config
    , botActionStateAdapter  :: a
    , botActionStatePayload  :: d
    }


type MatchedMessageData a = 
    ( "user" := User a
    , "channel" := Channel a
    , "match" := Match
    , "message" := Message 
    , "timestamp" := TimeStamp a
    )

type ChannelChangeData a =
    ( "user" := User a 
    , "channel" := Channel a 
    , "timestamp" := TimeStamp a
    )

type TopicChangeData a = 
    ( "user" := User a 
    , "channel" := Channel a 
    , "timestamp" := TimeStamp a
    , "topic" := Topic
    )

type FileSharedData a =
    ( "user" := User a 
    , "channel" := Channel a 
    , "timestamp" := TimeStamp a
    , "file" := RemoteFile a
    )

-- | Accumulator and sorting for all kinds of handlers
data Handlers a = Handlers
    { handlersResponds      :: Vector (Regex, MatchedMessageData a -> RunnerM ())
    , handlersHears         :: Vector (Regex, MatchedMessageData a -> RunnerM ())
    , handlersCustoms       :: Vector (Event a -> Maybe (RunnerM ()))
    , handlersJoins         :: Vector (ChannelChangeData a -> RunnerM ())
    , handlersLeaves        :: Vector (ChannelChangeData a -> RunnerM ())
    , handlersTopicChange   :: Vector (TopicChangeData a -> RunnerM ())
    , handlersFileShares    :: Vector (FileSharedData a -> RunnerM ())
    , handlersJoinsIn       :: HM.HashMap L.Text (Vector (ChannelChangeData a -> RunnerM ()))
    , handlersLeavesFrom    :: HM.HashMap L.Text (Vector (ChannelChangeData a -> RunnerM ()))
    , handlersTopicChangeIn :: HM.HashMap L.Text (Vector (TopicChangeData a -> RunnerM ()))
    , handlersFileSharesIn  :: HM.HashMap L.Text (Vector (FileSharedData a -> RunnerM ()))
    } deriving Generic

makeFields ''BotActionState
makeFields ''Handlers

instance NFData (Handlers a)



instance Monoid (Handlers a) where
    mempty = Handlers mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

#if HAS_SEMIGROUP
    mappend = (<>)

instance Semigroup (Handlers a) where

  Handlers r1 h1 c1 j1 l1 t1 fs1 ji1 li1 ti1 fsi1
    <> Handlers r2 h2 c2 j2 l2 t2 fs2 ji2 li2 ti2 fsi2
    = Handlers (r1 `mappend` r2) (h1 `mappend` h2) (c1 `mappend` c2) (j1 `mappend` j2) (l1 `mappend` l2) (t1 `mappend` t2) (fs1 `mappend` fs2)
    (HM.unionWith mappend ji1 ji2) (HM.unionWith mappend li1 li2) (HM.unionWith mappend ti1 ti2) (HM.unionWith mappend fsi1 fsi2)

#endif


-- | Context for reacting in the bot. Allows use of functions like 'Marvin.send', 'Marvin.reply' and 'Marvin.messageChannel', arbitrary 'IO' actions using 'liftIO' and logging via the 'MonadLogger' typeclass.
--
-- The type parameter @d@ is the accessible data provided by the trigger for this action and can be obtained with 'Marvin.getData' or other custom functions like 'Marvin.getMessage' and 'Marvin.getMatch' which typically depend on a particular type of data in @d@.
--
-- For completeness: @a@ is the adapter type and @r@ is the return type of the monadic computation.
--
-- This is also a 'MonadReader' instance, there you can inspect the entire state of this reaction.
-- This is typically only used in internal or utility functions and not necessary for the user.
-- To inspect particular pieces of this state refer to the *Lenses* section.
newtype BotReacting a d r = BotReacting { runReaction :: ReaderT (BotActionState a d) RunnerM r }
    deriving (Monad, MonadIO, Applicative, Functor, MonadReader (BotActionState a d), MonadLogger, MonadLoggerIO, MonadBase IO)

-- | An abstract type describing a marvin script using @a@ as adapter.
--
-- This is basically a collection of event handlers.
data Script a = Script
    { scriptActions  :: Handlers a
    , scriptScriptId :: ScriptId
    , scriptConfig   :: C.Config
    , scriptAdapter  :: a
    }

makeFields ''Script


-- | A monad for gradually defining a 'Script' using 'Marvin.respond' and 'Marvin.hear' as well as any 'IO' action.
newtype ScriptDefinition a r = ScriptDefinition { runScript :: StateT (Script a) RunnerM r }
    deriving (Monad, MonadIO, Applicative, Functor, MonadLogger, MonadLoggerIO, MonadBase IO)


-- | Initializer for a script. This gets run by the server during startup and creates a 'Script'
newtype ScriptInit a = ScriptInit (ScriptId, a -> C.Config -> RunnerM (Script a))


instance MonadBaseControl IO (AdapterM a) where
    type StM (AdapterM a) r = r
    liftBaseWith f = AdapterM $ liftBaseWith $ \q -> f (q . runAdapterAction)
    restoreM = AdapterM . restoreM


instance MonadBaseControl IO (BotReacting a d) where
    type StM (BotReacting a d) r = r
    liftBaseWith f = BotReacting $ liftBaseWith $ \q -> f (q . runReaction)
    restoreM = BotReacting . restoreM


-- | Similar to 'AccessAdapter', this class says there is a 'ScriptId' reachable from the type (usually a monad) @m@.
class IsScript m where
    -- | Retrieve the script id out of @m@, ususally a monad.
    getScriptId :: m ScriptId

instance IsScript (ScriptDefinition a) where
    getScriptId = ScriptDefinition $ use scriptId

instance IsScript (BotReacting a b) where
    getScriptId = view scriptId

-- | Lifting class for adapter actions.
--
-- The 'IsAdapter' and 'Monad' constraint is just for convenience
class (IsAdapter (AdapterT m), Monad m) => MonadAdapter m where
    type AdapterT m
    -- | Lift an action from the adapter context into the Monad @m@.
    liftAdapterM :: AdapterM (AdapterT m) r -> m r

genericLiftAdapterM :: (MonadLoggerIO m, HasConfigAccess m) => m (AdapterT m) -> AdapterM (AdapterT m) r -> m r
genericLiftAdapterM getAdapter (AdapterM ac) = do
    logger <- askLoggerIO
    liftIO . flip runLoggingT logger . runReaderT ac =<< AdapterMEnv <$> getConfigInternal <*> getAdapter

instance IsAdapter a => MonadAdapter (ScriptDefinition a) where
    type AdapterT (ScriptDefinition a) = a
    liftAdapterM = genericLiftAdapterM (ScriptDefinition $ use adapter)

instance IsAdapter a => MonadAdapter (BotReacting a b) where
    type AdapterT (BotReacting a b) = a
    liftAdapterM = genericLiftAdapterM (view adapter)

instance IsAdapter a => MonadAdapter (AdapterM a) where
    type AdapterT (AdapterM a) = a
    liftAdapterM = id

-- | Denotes a place from which we may access the configuration.
--
-- During script definition or when handling a request we can obtain the config with 'getConfigVal' or 'requireConfigVal'.
class (IsScript m, MonadIO m) => HasConfigAccess m where
    -- | INTERNAL USE WITH CARE
    --
    -- Obtain the entire config structure
    getConfigInternal :: m C.Config

instance HasConfigAccess (ScriptDefinition a) where
    getConfigInternal = ScriptDefinition $ use config

instance HasConfigAccess (BotReacting a b) where
    getConfigInternal = view config


instance ShowT ScriptId where showT = unwrapScriptId

instance ShowT (AdapterId a) where showT = unwrapAdapterId


type LoggingFn = Loc -> LogSource -> LogLevel -> LogStr -> IO ()


-- | Common base verification function for 'ScriptId' and 'AdapterId'
verifyIdString :: String -> (T.Text -> a) -> T.Text -> Either String a
verifyIdString thingToVerify f s =
  case T.uncons s of
    Nothing -> Left $(isS "#{thingToVerify} must not be empty")
    Just (x, xs)
      | isLetter x && T.all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs -> Right $ f s
      | otherwise -> Left $(isS "first character of #{thingToVerify} must be a letter, all other characters can be alphanumeric, '-' or '_'")


instance IsString ScriptId where
    fromString = either error id . mkScriptId . fromString


instance IsString (AdapterId a) where
    fromString = either error id . mkAdapterId . fromString


-- | Attempt to create a script id from 'Text'
mkScriptId :: T.Text -> Either String ScriptId
mkScriptId = verifyIdString "script id" ScriptId


-- | Attempt to create an adapter id from 'Text'
mkAdapterId :: T.Text -> Either String (AdapterId a)
mkAdapterId = verifyIdString "adapter id" AdapterId

newtype LogLevel' = LogLevel' { unwrapLogLevel' :: LogLevel }

instance C.Configured LogLevel' where
    convert (C.String s) =
        case T.strip $ T.toLower s of
            "debug"   -> ll LevelDebug
            "warning" -> ll LevelWarn
            "error"   -> ll LevelError
            "info"    -> ll LevelInfo
            _         -> Nothing
      where ll = return . LogLevel'
    convert _            = Nothing
