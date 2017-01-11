{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module Marvin.Internal.Types where


import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control
import           Data.Char                   (isAlphaNum, isLetter)
import qualified Data.Configurator.Types     as C
import qualified Data.HashMap.Strict         as HM
import           Data.Monoid
import           Data.String
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Time.Clock
import           Data.Vector                 (Vector)
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex



type Topic = L.Text
type Message = L.Text

newtype TimeStamp = TimeStamp { unwrapTimeStamp :: UTCTime } deriving Show


-- | Representation for the types of events which can occur
data Event a
    = MessageEvent (User a) (Channel a) Message TimeStamp
    | CommandEvent (User a) (Channel a) Message TimeStamp
    | ChannelJoinEvent (User a) (Channel a) TimeStamp
    | ChannelLeaveEvent (User a) (Channel a) TimeStamp
    | TopicChangeEvent (User a) (Channel a) Topic TimeStamp


type RunnerM = LoggingT IO

newtype AdapterM a r = AdapterM { runAdapterAction :: ReaderT (C.Config, a) RunnerM r } deriving (MonadIO, Monad, Applicative, Functor, MonadLogger, MonadLoggerIO, MonadBase IO)


type EventHandler a = Event a -> IO ()
type RunWithAdapter a = EventHandler a -> AdapterM a ()

-- | Basic functionality required of any adapter
class IsAdapter a where
    type User a
    type Channel a
    -- | Used for scoping config and logging
    adapterId :: AdapterId a
    -- | Post a message to a channel given the internal channel identifier
    messageChannel :: Channel a -> L.Text -> AdapterM a ()
    -- | Initialize the adapter state
    initAdapter :: RunnerM a
    -- | Initialize and run the bot
    runWithAdapter :: RunWithAdapter a
    -- | Resolve a username given the internal user identifier
    getUsername :: User a -> AdapterM a L.Text
    -- | Resolve the human readable name for a channel given the  internal channel identifier
    getChannelName :: Channel a -> AdapterM a L.Text
    -- | Resolve to the internal channel identifier given a human readable name
    resolveChannel :: L.Text -> AdapterM a (Maybe (Channel a))


newtype User' a = User' {unwrapUser' :: User a}
newtype Channel' a = Channel' {unwrapChannel' :: Channel a}


-- | A type, basically a String, which identifies a script to the config and the logging facilities.
newtype ScriptId = ScriptId { unwrapScriptId :: T.Text } deriving (Show, Eq)


-- | A type, basically a String, which identifies an adapter to the config and the logging facilities.
newtype AdapterId a = AdapterId { unwrapAdapterId :: T.Text } deriving (Show, Eq)


class HasScriptId s a | s -> a where
    scriptId :: Lens' s a


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
        { handlersResponds :: Vector (Regex, (User' a, Channel' a, Match, Message, TimeStamp) -> RunnerM ())
        , handlersHears :: Vector (Regex, (User' a, Channel' a, Match, Message, TimeStamp) -> RunnerM ())
        , handlersCustoms :: Vector (Event a -> Maybe (RunnerM ()))
        , handlersJoins :: Vector ((User' a, Channel' a, TimeStamp) -> RunnerM ())
        , handlersLeaves :: Vector ((User' a, Channel' a, TimeStamp) -> RunnerM ())
        , handlersTopicChange :: Vector ((User' a, Channel' a, Topic, TimeStamp) -> RunnerM ())
        , handlersJoinsIn :: HM.HashMap L.Text (Vector ((User' a, Channel' a, TimeStamp) -> RunnerM ()))
        , handlersLeavesFrom :: HM.HashMap L.Text (Vector ((User' a, Channel' a, TimeStamp) -> RunnerM ()))
        , handlersTopicChangeIn :: HM.HashMap L.Text (Vector ((User' a, Channel' a, Topic, TimeStamp) -> RunnerM ()))
        }
    |]


instance Monoid (Handlers a) where
    mempty = Handlers mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend (Handlers r1 h1 c1 j1 l1 t1 ji1 li1 ti1)
            (Handlers r2 h2 c2 j2 l2 t2 ji2 li2 ti2)
        = Handlers (r1 <> r2) (h1 <> h2) (c1 <> c2) (j1 <> j2) (l1 <> l2) (t1 <> t2) (HM.unionWith mappend ji1 ji2) (HM.unionWith mappend li1 li2) (HM.unionWith mappend ti1 ti2)



-- | Monad for reacting in the bot. Allows use of functions like 'send', 'reply' and 'messageChannel' as well as any arbitrary 'IO' action using 'liftIO'.
--
-- The type parameter @d@ is the accessible data provided by the trigger for this action and can be obtained with 'getData' or other custom functions like 'getMessage' and 'getMatch' which typically depend on a particular type of data in @d@.
-- For message handlers like 'hear' and 'respond' this would be a regex 'Match' and a 'Message' for instance.
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


-- | A monad for gradually defining a 'Script' using 'respond' and 'hear' as well as any 'IO' action.
newtype ScriptDefinition a r = ScriptDefinition { runScript :: StateT (Script a) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadLogger, MonadBase IO)


-- | Initializer for a script. This gets run by the server during startup and creates a 'Script'
newtype ScriptInit a = ScriptInit (ScriptId, a -> C.Config -> RunnerM (Script a))


instance MonadBaseControl IO (AdapterM a) where
    type StM (AdapterM a) r = r
    liftBaseWith f = AdapterM $ liftBaseWith $ \q -> f (q . runAdapterAction)
    restoreM = AdapterM . restoreM



-- | Class which says that there is a way to get to a 'Message' from this type @m@.
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

instance Get (a, b, TimeStamp) TimeStamp where
    getLens = _3

instance Get (a, b, c, TimeStamp) TimeStamp where
    getLens = _4

instance Get (a, b, c, d, TimeStamp) TimeStamp where
    getLens = _5

instance Get (a, b, Match, d, e) Match where
    getLens = _3

instance Get (a, b, c, Message, e) Message where
    getLens = _4

instance Get (a, b, Topic, d) Topic where
    getLens = _3

instance HasConfigAccess (ScriptDefinition a) where
    getConfigInternal = ScriptDefinition $ use config

instance HasConfigAccess (BotReacting a b) where
    getConfigInternal = view config


class IsScript m where
    getScriptId :: m ScriptId

instance IsScript (ScriptDefinition a) where
    getScriptId = ScriptDefinition $ use scriptId

instance IsScript (BotReacting a b) where
    getScriptId = view scriptId


class AccessAdapter m where
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
    getAdapter = AdapterM $ snd <$> ask

instance ShowT ScriptId where showT = unwrapScriptId

instance ShowT (AdapterId a) where showT = unwrapAdapterId


type LoggingFn = Loc -> LogSource -> LogLevel -> LogStr -> IO ()


verifyIdString :: String -> (String -> a) -> String -> a
verifyIdString name _ "" = error $ name ++ " must not be empty"
verifyIdString name f s@(x:xs)
    | isLetter x && all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs = f s
    | otherwise = error $ "first character of " ++ name ++ " must be a letter, all other characters can be alphanumeric, '-' or '_'"


instance IsString ScriptId where
    fromString = verifyIdString "script id" (ScriptId . fromString)


instance IsString (AdapterId a) where
    fromString = verifyIdString "adapter id" (AdapterId . fromString)


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
            "debug" -> Just LevelDebug
            "warning" -> Just LevelWarn
            "error" -> Just LevelError
            "info" -> Just LevelInfo
            _ -> Nothing
    convert _            = Nothing

