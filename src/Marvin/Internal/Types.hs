{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvin.Internal.Types where


import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char               (isAlphaNum, isLetter)
import qualified Data.Configurator.Types as C
import           Data.String
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as L
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Marvin.Interpolate.Text
import           Text.Read               (readMaybe)
import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control


type Topic = L.Text
type Message = L.Text

-- | Representation for the types of events which can occur
data Event a
    = MessageEvent (User a) (Channel a) Message TimeStamp
    | CommandEvent (User a) (Channel a) Message TimeStamp
    | ChannelJoinEvent (User a) (Channel a) TimeStamp
    | ChannelLeaveEvent (User a) (Channel a) TimeStamp
    | TopicChangeEvent (User a) (Channel a) Topic TimeStamp


newtype AdapterM a r = AdapterM { runAdapterAction :: ReaderT (C.Config, a) RunnerM r } deriving (MonadIO, Monad, Applicative, Functor, MonadLogger, MonadLoggerIO, MonadBase IO)

instance MonadBaseControl IO (AdapterM a) where
    type StM (AdapterM a) r = r
    liftBaseWith f = AdapterM $ liftBaseWith $ \q -> f (q . runAdapterAction)
    restoreM = AdapterM . restoreM

instance AccessAdapter (AdapterM a) where
    type AdapterT (AdapterM a) = a
    getAdapter = AdapterM $ snd <$> ask

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

newtype TimeStamp = TimeStamp { unwrapTimeStamp :: UTCTime } deriving Show


timestampFromNumber :: Value -> Parser TimeStamp
timestampFromNumber (Number n) = return $ TimeStamp $ posixSecondsToUTCTime $ realToFrac n
timestampFromNumber (String s) = maybe mzero (return . TimeStamp . posixSecondsToUTCTime . realToFrac) $ readMaybe (T.unpack s) 
timestampFromNumber _ = mzero
        


-- | A type, basically a String, which identifies a script to the config and the logging facilities.
newtype ScriptId = ScriptId { unwrapScriptId :: T.Text } deriving (Show, Eq)


-- | A type, basically a String, which identifies an adapter to the config and the logging facilities.
newtype AdapterId a = AdapterId { unwrapAdapterId :: T.Text } deriving (Show, Eq)


instance ShowT ScriptId where showT = unwrapScriptId

instance ShowT (AdapterId a) where showT = unwrapAdapterId


applicationScriptId :: ScriptId
applicationScriptId = ScriptId "bot"


type RunnerM = LoggingT IO


verifyIdString :: String -> (String -> a) -> String -> a
verifyIdString name _ "" = error $ name ++ " must not be empty"
verifyIdString name f s@(x:xs)
    | isLetter x && all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs = f s
    | otherwise = error $ "first character of " ++ name ++ " must be a letter, all other characters can be alphanumeric, '-' or '_'"


instance IsString ScriptId where
    fromString = verifyIdString "script id" (ScriptId . fromString)


instance IsString (AdapterId a) where
    fromString = verifyIdString "adapter id" (AdapterId . fromString)


class HasScriptId s a | s -> a where
    scriptId :: Lens' s a


-- | Denotes a place from which we may access the configuration.
--
-- During script definition or when handling a request we can obtain the config with 'getConfigVal' or 'requireConfigVal'.
class (IsScript m, MonadIO m) => HasConfigAccess m where
    -- | INTERNAL USE WITH CARE
    --
    -- Obtain the entire config structure
    getConfigInternal :: m C.Config


class IsScript m where
    getScriptId :: m ScriptId

instance C.Configured LogLevel where
    convert (C.String s) =
        case T.strip $ T.toLower s of
            "debug" -> Just LevelDebug
            "warning" -> Just LevelWarn
            "error" -> Just LevelError
            "info" -> Just LevelInfo
            _ -> Nothing
    convert _            = Nothing

class AccessAdapter m where
    type AdapterT m
    getAdapter :: m (AdapterT m)
