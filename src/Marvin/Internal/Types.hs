{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module Marvin.Internal.Types where


import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.ByteString.Lazy        (ByteString)
import           Data.Char                   (isAlphaNum, isLetter)
import qualified Data.Configurator.Types     as C
import qualified Data.HashMap.Strict         as HM
import           Data.Monoid
import           Data.String
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Time.Clock
import           Data.Vector                 (Vector)
import           GHC.Generics
import           Marvin.Internal.LensClasses
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex

-- | The topic in a channel
type Topic = L.Text
-- | The contents of a recieved message
type Message = L.Text

-- | A timestamp type. Supplied with most 'Event' types.
--
-- The type parameter is inly a tag. It is used so you may declare instances for classes such as 'FromJSON' for this type depending on your adapter. So that different adapters may have different 'FromJSON' instances for the timestamp.
newtype TimeStamp a = TimeStamp { unwrapTimeStamp :: UTCTime } deriving Show


declareFields [d|
    data AdapterMEnv a = AdapterMEnv
        { adapterMEnvConfig :: C.Config
        , adapterMEnvAdapter :: a
        }
    |]


-- | Representation for the types of events which can occur
data Event a
    = MessageEvent (User a) (Channel a) Message (TimeStamp a)
    | CommandEvent (User a) (Channel a) Message (TimeStamp a)
    | ChannelJoinEvent (User a) (Channel a) (TimeStamp a)
    | ChannelLeaveEvent (User a) (Channel a) (TimeStamp a)
    | TopicChangeEvent (User a) (Channel a) Topic (TimeStamp a)
    | HasFiles a => FileSharedEvent (User a) (Channel a) (RemoteFile a) (TimeStamp a)

-- | Basic monad which most internal actions run in
type RunnerM = LoggingT IO

type EventConsumer a = Event a -> AdapterM a ()

-- | Basic functionality required of any adapter
class ( HasUsername (User a) L.Text
      , HasName (User a) (Maybe L.Text)
      , HasFirstName (User a) (Maybe L.Text)
      , HasLastName (User a) (Maybe L.Text)
      , HasName (Channel a) L.Text
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
newtype AdapterM a r = AdapterM { runAdapterAction :: ReaderT (AdapterMEnv a) RunnerM r } deriving (MonadIO, Monad, Applicative, Functor, MonadLogger, MonadLoggerIO, MonadBase IO, MonadReader (AdapterMEnv a))


data FileContent = FileOnDisk L.Text | FileInMemory ByteString


class ( HasName (RemoteFile a) (Maybe L.Text)
      , HasUrl (RemoteFile a) (Maybe L.Text)
      , HasFileType (RemoteFile a) (Maybe L.Text)
      , HasCreationDate (RemoteFile a) (TimeStamp a)
      , HasSize (RemoteFile a) Integer
      , HasContent (LocalFile a) FileContent
      , HasName (LocalFile a) (Maybe L.Text)
      , HasFileType (LocalFile a) (Maybe L.Text)
      ) => HasFiles a where
    -- | Concrete type of an uploaded file
    type RemoteFile a
    type LocalFile a
    -- | Resolve the name of the file
    newLocalFile :: FileContent -> AdapterM a (LocalFile a)
    readTextFile :: RemoteFile a -> AdapterM a (Maybe L.Text)
    readFileBytes :: RemoteFile a -> AdapterM a (Maybe ByteString)
    shareFile :: LocalFile a -> [Channel a] -> AdapterM a ()


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
declareFields [d|
    data BotActionState a d = BotActionState
        { botActionStateScriptId :: ScriptId
        , botActionStateConfig   :: C.Config
        , botActionStateAdapter :: a
        , botActionStatePayload :: d
        }
    |]


declareFields [d|
    data Handlers a = Handlers
        { handlersResponds :: Vector (Regex, (User' a, Channel' a, Match, Message, TimeStamp a) -> RunnerM ())
        , handlersHears :: Vector (Regex, (User' a, Channel' a, Match, Message, TimeStamp a) -> RunnerM ())
        , handlersCustoms :: Vector (Event a -> Maybe (RunnerM ()))
        , handlersJoins :: Vector ((User' a, Channel' a, TimeStamp a) -> RunnerM ())
        , handlersLeaves :: Vector ((User' a, Channel' a, TimeStamp a) -> RunnerM ())
        , handlersTopicChange :: Vector ((User' a, Channel' a, Topic, TimeStamp a) -> RunnerM ())
        , handlersFileShares :: Vector ((User' a, Channel' a, RemoteFile' a, TimeStamp a) -> RunnerM ())
        , handlersJoinsIn :: HM.HashMap L.Text (Vector ((User' a, Channel' a, TimeStamp a) -> RunnerM ()))
        , handlersLeavesFrom :: HM.HashMap L.Text (Vector ((User' a, Channel' a, TimeStamp a) -> RunnerM ()))
        , handlersTopicChangeIn :: HM.HashMap L.Text (Vector ((User' a, Channel' a, Topic, TimeStamp a) -> RunnerM ()))
        , handlersFileSharesIn :: HM.HashMap L.Text (Vector ((User' a, Channel' a, RemoteFile' a, TimeStamp a) -> RunnerM ()))
        } deriving Generic
    |]


instance NFData (Handlers a)


instance Monoid (Handlers a) where
    mempty = Handlers mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend (Handlers r1 h1 c1 j1 l1 t1 fs1 ji1 li1 ti1 fsi1)
            (Handlers r2 h2 c2 j2 l2 t2 fs2 ji2 li2 ti2 fsi2)
        = Handlers (r1 <> r2) (h1 <> h2) (c1 <> c2) (j1 <> j2) (l1 <> l2) (t1 <> t2) (fs1 <> fs2) (HM.unionWith mappend ji1 ji2) (HM.unionWith mappend li1 li2) (HM.unionWith mappend ti1 ti2) (HM.unionWith mappend fsi1 fsi2)



-- | Monad for reacting in the bot. Allows use of functions like 'Marvin.send', 'Marvin.reply' and 'Marvin.messageChannel' as well as any arbitrary 'IO' action using 'liftIO'.
--
-- The type parameter @d@ is the accessible data provided by the trigger for this action and can be obtained with 'Marvin.getData' or other custom functions like 'Marvin.getMessage' and 'Marvin.getMatch' which typically depend on a particular type of data in @d@.
--
-- For completeness: @a@ is the adapter type and @r@ is the return type of the monadic computation.
--
-- This is also a 'MonadReader' instance, there you can inspect the entire state of this reaction.
-- This is typically only used in internal or utility functions and not necessary for the user.
-- To inspect particular pieces of this state refer to the *Lenses* section.
newtype BotReacting a d r = BotReacting { runReaction :: ReaderT (BotActionState a d) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadReader (BotActionState a d), MonadLogger, MonadLoggerIO, MonadBase IO)

-- | An abstract type describing a marvin script.
--
-- This is basically a collection of event handlers.
--
-- Internal structure is exposed for people wanting to extend this.
declareFields [d|
    data Script a = Script
        { scriptActions   :: Handlers a
        , scriptScriptId  :: ScriptId
        , scriptConfig    :: C.Config
        , scriptAdapter :: a
        }
    |]


-- | A monad for gradually defining a 'Script' using 'Marvin.respond' and 'Marvin.hear' as well as any 'IO' action.
newtype ScriptDefinition a r = ScriptDefinition { runScript :: StateT (Script a) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadLogger, MonadBase IO)


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


-- | Class which says that there is a way to get to @b@ from this type @a@.
--
-- This typeclass is used to allow handlers with different types of payload to share common
-- accessor functions such as 'Marvin.getUser' and 'Marvin.getMessage'.
--
-- The instances specify for each type of payload which pieces of data can be extracted and how.
class Get a b where
    getLens :: Lens' a b

instance Get (User' a, b, c) (User' a) where
    getLens = _1

instance Get (User' a, b, c, d) (User' a) where
    getLens = _1

instance Get (User' a, b, c, d, e) (User' a) where
    getLens = _1

instance Get (a, Channel' b, c) (Channel' b) where
    getLens = _2

instance Get (a, Channel' b, c, d) (Channel' b) where
    getLens = _2

instance Get (a, Channel' b, c, d, e) (Channel' b) where
    getLens = _2

instance Get (a, b, TimeStamp c) (TimeStamp c) where
    getLens = _3

instance Get (a, b, c, TimeStamp d) (TimeStamp d) where
    getLens = _4

instance Get (a, b, c, d, TimeStamp e) (TimeStamp e) where
    getLens = _5

instance Get (a, b, Match, d, e) Match where
    getLens = _3

instance Get (a, b, c, Message, e) Message where
    getLens = _4

instance Get (a, b, Topic, d) Topic where
    getLens = _3

instance Get (a, b, RemoteFile' c, d) (RemoteFile' c) where
    getLens = _3

instance HasConfigAccess (ScriptDefinition a) where
    getConfigInternal = ScriptDefinition $ use config

instance HasConfigAccess (BotReacting a b) where
    getConfigInternal = view config

-- | Similar to 'AccessAdapter', this class says there is a 'ScriptId' reachable from the type (usually a monad) @m@.
class IsScript m where
    -- | Retrieve the script id out of @m@, ususally a monad.
    getScriptId :: m ScriptId

instance IsScript (ScriptDefinition a) where
    getScriptId = ScriptDefinition $ use scriptId

instance IsScript (BotReacting a b) where
    getScriptId = view scriptId


-- | Similar to 'IsScript', this class says that there is an adapter 'AdapterT' available from this type (usually a monad) @m@.
--
-- The type of adapter depends on the monad itself.
-- This class can be thought of as 'MonadReader' specified to 'AdapterT'.
class AccessAdapter m where
    -- | The concrete type of adapter accessible from @m@.
    type AdapterT m
    getAdapter :: m (AdapterT m)

instance AccessAdapter (ScriptDefinition a) where
    type AdapterT (ScriptDefinition a) = a
    getAdapter = ScriptDefinition $ use adapter

instance AccessAdapter (BotReacting a b) where
    type AdapterT (BotReacting a b) = a
    getAdapter = view adapter


instance AccessAdapter (AdapterM a) where
    type AdapterT (AdapterM a) = a
    getAdapter = view adapter

instance ShowT ScriptId where showT = unwrapScriptId

instance ShowT (AdapterId a) where showT = unwrapAdapterId


type LoggingFn = Loc -> LogSource -> LogLevel -> LogStr -> IO ()


verifyIdString :: String -> (T.Text -> a) -> T.Text -> Either String a
verifyIdString name _ "" = Left $(isS "#{name} must not be empty")
verifyIdString name f s
    | isLetter x && T.all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs = Right $ f s
    | otherwise = Left $(isS "first character of #{name} must be a letter, all other characters can be alphanumeric, '-' or '_'")
  where Just (x, xs) = T.uncons s


instance IsString ScriptId where
    fromString = either error id . verifyIdString "script id" ScriptId . fromString


instance IsString (AdapterId a) where
    fromString = either error id . verifyIdString "adapter id" AdapterId . fromString


-- | Attempt to create a script id from 'Text'
mkScriptId :: T.Text -> Either String ScriptId
mkScriptId = verifyIdString "script id" ScriptId


-- | Attempt to create an adapter id from 'Text'
mkAdapterId :: T.Text -> Either String (AdapterId a)
mkAdapterId = verifyIdString "adapter id" AdapterId


-- | Denotes a place from which we may access the configuration.
--
-- During script definition or when handling a request we can obtain the config with 'getConfigVal' or 'requireConfigVal'.
class (IsScript m, MonadIO m) => HasConfigAccess m where
    -- | INTERNAL USE WITH CARE
    --
    -- Obtain the entire config structure
    getConfigInternal :: m C.Config


instance C.Configured LogLevel where
    convert (C.String s) =
        case T.strip $ T.toLower s of
            "debug"   -> Just LevelDebug
            "warning" -> Just LevelWarn
            "error"   -> Just LevelError
            "info"    -> Just LevelInfo
            _         -> Nothing
    convert _            = Nothing

