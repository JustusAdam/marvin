{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}
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
import           Marvin.Util.Logging
import           Marvin.Util.Regex       (Match, Regex)


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


data WrappedAction a = forall d. WrappedAction (ActionData d) (BotReacting a d ())


-- | Monad for reacting in the bot. Allows use of functions like 'send', 'reply' and 'messageChannel' as well as any arbitrary 'IO' action.
--
-- The type parameter @d@ is the accessible data provided by the trigger for this action.
-- For message handlers like 'hear' and 'respond' this would be a regex 'Match' and a 'Message' for instance.
newtype BotReacting a d r = BotReacting { runReaction :: ReaderT (BotActionState a d) IO r } deriving (Monad, MonadIO, Applicative, Functor, MonadReader (BotActionState a d))

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
newtype ScriptDefinition a r = ScriptDefinition { runScript :: StateT (Script a) IO r } deriving (Monad, MonadIO, Applicative, Functor)


-- | Initializer for a script. This gets run by the server during startup and creates a 'Script'
newtype ScriptInit a = ScriptInit (ScriptId, a -> C.Config -> IO (Script a))


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


-- | Send a message to the channel the triggering message came from.
--
-- Equivalent to "robot.send" in hubot
send :: (IsAdapter a, HasMessage m) => String -> BotReacting a m ()
send msg = do
    o <- getMessage
    messageChannel' (channel o) msg


-- | Get the username of a registered user.
getUsername :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => User -> m String
getUsername usr = do
    a <- getAdapter
    liftIO $ A.getUsername a usr


resolveChannel :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => String -> m (Maybe Channel)
resolveChannel name = do
    a <- getAdapter
    liftIO $ A.resolveChannel a name


-- | Get the human readable name of a channel.
getChannelName :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel -> m String
getChannelName rm = do
    a <- getAdapter
    liftIO $ A.getChannelName a rm


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: (IsAdapter a, HasMessage m) => String -> BotReacting a m ()
reply msg = do
    om <- getMessage
    user <- getUsername $ sender om
    send $ user ++ " " ++ msg


-- | Send a message to a Channel (by name)
messageChannel :: (AccessAdapter m, IsAdapter (AdapterT m), IsScript m, MonadIO m) => String -> String -> m ()
messageChannel name msg = do
    mchan <- resolveChannel name
    maybe (errorM $ "No channel known with the name " ++ name) (`messageChannel'` msg) mchan


messageChannel' :: (AccessAdapter m, IsAdapter (AdapterT m), IsScript m, MonadIO m) => Channel -> String -> m ()
messageChannel' chan msg = do
    a <- getAdapter
    liftIO $ A.messageChannel a chan msg



-- | Define a new script for marvin
--
-- You need to provide a ScriptId (which can simple be written as a non-empty string).
-- This id is used as the key for the section in the bot config belonging to this script and in logging output.
--
-- Roughly equivalent to "module.exports" in hubot.
defineScript :: ScriptId -> ScriptDefinition a () -> ScriptInit a
defineScript sid definitions =
    ScriptInit (sid, runDefinitions sid definitions)


runDefinitions :: ScriptId -> ScriptDefinition a () -> a -> C.Config -> IO (Script a)
runDefinitions sid definitions ada cfg = execStateT (runScript definitions) (Script mempty sid cfg ada)


-- | Obtain the reaction dependent data from the bot.
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
    return $ runReaderT (runReaction reac) s


-- | Take an action and produce an IO action with the same effect.
-- Useful for creating actions which can be scheduled to execute a certain time or asynchronous.
-- The idea is that one can conveniently send messages from inside a schedulable action.
extractAction :: BotReacting a () o -> ScriptDefinition a (IO o)
extractAction ac = ScriptDefinition $ do
    a <- use adapter
    sid <- use scriptId
    cfg <- use config
    return $ runReaderT (runReaction ac) (BotActionState sid cfg a ())
