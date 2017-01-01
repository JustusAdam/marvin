{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances       #-}
module Marvin.Internal where


import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C

import           Control.Lens            hiding (cons)
import           Data.Monoid             ((<>))
import           Data.Sequences
import           Marvin.Adapter          (IsAdapter)
import qualified Marvin.Adapter          as A
import           Marvin.Internal.Types
import           Marvin.Util.Regex       (Match, Regex)
import Util
import Control.Monad.Logger
import qualified Data.Text.Lazy as L
import Control.Monad.Trans
import Marvin.Interpolate.Text


-- | Read only data available to a handler when the bot reacts to an event.
declareFields [d|
    data BotActionState a d = BotActionState
        { botActionStateScriptId :: ScriptId
        , botActionStateConfig   :: C.Config
        , botActionStateAdapter :: a
        , botActionStateVariable :: d
        }
    |]

-- | Payload in the reaction Monad when triggered by a message.
--  Contains a field for the 'Message' and a field for the 'Match' from the 'Regex'.
--
-- Both fields are accessible directly via the 'getMessage' and 'getMatch' functions
-- or this data via 'getData'.
declareFields [d|
    data MessageReactionData = MessageReactionData
        { messageReactionDataMessageField :: Message
        , messageReactionDataMatchField :: Match
        }
    |]


data ActionData d where
    Hear :: Regex -> ActionData MessageReactionData
    Respond :: Regex -> ActionData MessageReactionData
    Custom :: (A.Event -> Maybe d) -> ActionData d


data WrappedAction a = forall d. WrappedAction (ActionData d) (BotReacting a d ())


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
newtype BotReacting a d r = BotReacting { runReaction :: ReaderT (BotActionState a d) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadReader (BotActionState a d), MonadLogger)

-- | An abstract type describing a marvin script.
--
-- This is basically a collection of event handlers.
--
-- Internal structure is exposed for people wanting to extend this.
declareFields [d|
    data Script a = Script
        { scriptActions   :: [WrappedAction a]
        , scriptScriptId  :: ScriptId
        , scriptConfig    :: C.Config
        , scriptAdapter :: a
        }
    |]


-- | A monad for gradually defining a 'Script' using 'respond' and 'hear' as well as any 'IO' action.
newtype ScriptDefinition a r = ScriptDefinition { runScript :: StateT (Script a) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadLogger)


-- | Initializer for a script. This gets run by the server during startup and creates a 'Script'
newtype ScriptInit a = ScriptInit (ScriptId, a -> C.Config -> RunnerM (Script a))


-- | Class which says that there is a way to get to a 'Message' from this type @m@.
class HasMessage m where
    messageLens :: Lens' m Message

instance HasMessageField m Message => HasMessage m where
    messageLens = messageField

-- | Class which says that there is a way to get to a 'Match' from this type @m@.
class HasMatch m where
    matchLens :: Lens' m Match

instance HasMatchField m Match => HasMatch m where
    matchLens = matchField

-- | Class which says that there is a way to get to a topic of type 'String' from this type @m@.
class HasTopic m where
    topicLens :: Lens' m L.Text

instance HasTopic (L.Text, a) where
    topicLens = _1

idLens :: Lens' a a
idLens = lens id (flip const)

instance HasTopic L.Text where
    topicLens = idLens

class HasChannel a where
    channelLens :: Lens' a Channel

instance HasChannel Channel where
    channelLens = idLens

instance HasChannel (a, Channel) where
    channelLens = _2

class HasUser a where
    userLens :: Lens' a User

instance HasUser User where
    userLens = idLens

instance HasUser (User, a) where
    userLens = _1

instance HasUser MessageReactionData where
    userLens = messageField . lens sender (\a b -> a {sender = b})

instance HasConfigAccess (ScriptDefinition a) where
    getConfigInternal = ScriptDefinition $ use config

instance HasConfigAccess (BotReacting a b) where
    getConfigInternal = view config

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

getSubConfFor :: HasConfigAccess m => ScriptId -> m C.Config
getSubConfFor (ScriptId name) = C.subconfig ("script." <> name) <$> getConfigInternal


getConfig :: HasConfigAccess m => m C.Config
getConfig = getScriptId >>= getSubConfFor


addReaction :: ActionData d -> BotReacting a d () -> ScriptDefinition a ()
addReaction data_ action = ScriptDefinition $ actions %= cons (WrappedAction data_ action)


-- | Whenever any message matches the provided regex this handler gets run.
--
-- Equivalent to "robot.hear" in hubot
hear :: Regex -> BotReacting a MessageReactionData () -> ScriptDefinition a ()
hear !re = addReaction (Hear re)


-- | Runs the handler only if the bot was directly addressed.
--
-- Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting a MessageReactionData () -> ScriptDefinition a ()
respond !re = addReaction (Respond re)


-- | This handler runs whenever a user enters __any channel__ (which the bot is subscribed to)
--
-- The payload contains the entering user and the channel which was entered.
enter :: BotReacting a (User, Channel) () -> ScriptDefinition a ()
enter = notImplemented


-- | This handler runs whenever a user exits __any channel__ (which the bot is subscribed to)
--
-- The payload contains the exiting user and the channel which was exited.
exit :: BotReacting a (User, Channel) () -> ScriptDefinition a ()
exit = notImplemented


-- | This handler runs whenever a user enters __the specified channel__.
--
-- The argument is the human readable name for the channel.
--
-- The payload contains the entering user.
enterIn :: String -> BotReacting a User () -> ScriptDefinition a ()
enterIn = notImplemented


-- | This handler runs whenever a user exits __the specified channel__, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable name for the channel.
--
-- The payload contains the exting user.
exitFrom :: String -> BotReacting a User () -> ScriptDefinition a ()
exitFrom = notImplemented


-- | This handler runs when the topic in __any channel__ the bot is subscribed to changes.
--
-- The payload contains the new topic and the channel in which it was set.
topic :: BotReacting a (String, Channel) () -> ScriptDefinition a ()
topic = notImplemented


-- | This handler runs when the topic in __the specified channel__ is changed, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable channel name.
topicIn :: String -> BotReacting a String () -> ScriptDefinition a ()
topicIn = notImplemented


-- | Extension point for the user
--
-- Allows you to handle the raw event yourself.
-- Returning 'Nothing' from the trigger function means you dont want to react to the event.
-- The value returned inside the 'Just' is available in the handler later using 'getData'.
customTrigger :: (A.Event -> Maybe d) -> BotReacting a d () -> ScriptDefinition a ()
customTrigger tr = addReaction (Custom tr)


-- | Send a message to the channel the triggering message came from.
--
-- Equivalent to "robot.send" in hubot
send :: (IsAdapter a, HasChannel m) => L.Text -> BotReacting a m ()
send msg = do
    o <- getChannel
    messageChannel' o msg


-- | Get the username of a registered user.
getUsername :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => User -> m L.Text
getUsername usr = do
    a <- getAdapter
    A.liftAdapterAction $ A.getUsername a usr


resolveChannel :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => L.Text -> m (Maybe Channel)
resolveChannel name = do
    a <- getAdapter
    A.liftAdapterAction $ A.resolveChannel a name


-- | Get the human readable name of a channel.
getChannelName :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel -> m L.Text
getChannelName rm = do
    a <- getAdapter
    A.liftAdapterAction $ A.getChannelName a rm


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: (IsAdapter a, HasMessage m) => L.Text -> BotReacting a m ()
reply msg = do
    om <- getMessage
    user <- getUsername $ sender om
    messageChannel' (channel om) $ user <> " " <> msg


-- | Send a message to a Channel (by name)
messageChannel :: (AccessAdapter m, IsAdapter (AdapterT m), IsScript m, MonadIO m, MonadLogger m) => L.Text -> L.Text -> m ()
messageChannel name msg = do
    mchan <- resolveChannel name
    maybe ($logError [iqT|No channel known with the name %{name}|]) (`messageChannel'` msg) mchan


messageChannel' :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel -> L.Text -> m ()
messageChannel' chan msg = do
    a <- getAdapter
    A.liftAdapterAction $ A.messageChannel a chan msg



-- | Define a new script for marvin
--
-- You need to provide a ScriptId (which can simple be written as a non-empty string).
-- This id is used as the key for the section in the bot config belonging to this script and in logging output.
--
-- Roughly equivalent to "module.exports" in hubot.
defineScript :: ScriptId -> ScriptDefinition a () -> ScriptInit a
defineScript sid definitions =
    ScriptInit (sid, runDefinitions sid definitions)


runDefinitions :: ScriptId -> ScriptDefinition a () -> a -> C.Config -> RunnerM (Script a)
runDefinitions sid definitions ada cfg = execStateT (runScript definitions) (Script mempty sid cfg ada)


-- | Obtain the event reaction data.
--
-- The type of this data depends on the reaction function used.
-- For instance 'hear' and 'respond' will contain 'MessageReactionData'.
-- The actual contents comes from the event itself and was put together by the trigger.
getData :: BotReacting a d d
getData = view variable


-- | Get the results from matching the regular expression.
--
-- Equivalent to "msg.match" in hubot.
getMatch :: HasMatch m => BotReacting a m Match
getMatch = view (variable . matchLens)


-- | Get the message that triggered this action
-- Includes sender, target channel, as well as the full, untruncated text of the original message
getMessage :: HasMessage m => BotReacting a m Message
getMessage = view (variable . messageLens)


-- | Get the the new topic.
getTopic :: HasTopic m => BotReacting a m L.Text
getTopic = view (variable . topicLens)


-- | Get the stored channel in which something happened.
getChannel :: HasChannel m => BotReacting a m Channel
getChannel = view (variable . channelLens)


-- | Get the user whihc was part of the triggered action.
getUser :: HasUser m => BotReacting a m User
getUser = view (variable . userLens)


-- | Get a value out of the config, returns 'Nothing' if the value didn't exist.
--
-- This config is the config for this script. Ergo all config vars registered under the config section for the ScriptId of this script.
--
-- The 'HasConfigAccess' Constraint means this function can be used both during script definition and when a handler is run.
getConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m (Maybe a)
getConfigVal name = do
    cfg <- getConfig
    liftIO $ C.lookup cfg name


-- | Get a value out of the config and fail with an error if the specified key is not found.
--
-- This config is the config for this script. Ergo all config vars registered under the config section for the ScriptId of this script.
--
-- The 'HasConfigAccess' Constraint means this function can be used both during script definition and when a handler is run.
requireConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m a
requireConfigVal name = do
    cfg <- getConfig
    liftIO $ C.require cfg name


getAppConfig :: HasConfigAccess m => m C.Config
getAppConfig = getSubConfFor applicationScriptId


getAppConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m (Maybe a)
getAppConfigVal name = do
    cfg <- getAppConfig
    liftIO $ C.lookup cfg name


requireAppConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m a
requireAppConfigVal name = do
    cfg <- getAppConfig
    liftIO $ C.require cfg name


-- | Take a reaction and produce an IO action with the same effect.
-- Useful for creating actions which can be scheduled to execute a certain time or asynchronous.
-- The idea is that one can conveniently send messages from inside a schedulable action.
extractReaction :: BotReacting a s o -> BotReacting a s (IO o)
extractReaction reac = BotReacting $ do
    s <- ask
    return $ runStderrLoggingT $ runReaderT (runReaction reac) s


-- | Take an action and produce an IO action with the same effect.
-- Useful for creating actions which can be scheduled to execute a certain time or asynchronous.
-- The idea is that one can conveniently send messages from inside a schedulable action.
extractAction :: BotReacting a () o -> ScriptDefinition a (IO o)
extractAction ac = ScriptDefinition $ do
    a <- use adapter
    sid <- use scriptId
    cfg <- use config
    return $ runStderrLoggingT $ runReaderT (runReaction ac) (BotActionState sid cfg a ())
