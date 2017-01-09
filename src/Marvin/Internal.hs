{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Marvin.Internal 
    ( 
    -- * Exposed API
    defineScript
    -- ** Reacting
    , hear, respond, topic, topicIn, enter, exit, enterIn, exitFrom, customTrigger
    -- ** Sending messages
    , send, reply, messageChannel, messageChannel'
    -- ** Getting Data
    , getData, getUser, getMatch, getMessage, getChannel, getTopic, getBotName, getChannelName, resolveChannel, getUsername
    -- ** Interacting with the config
    , getConfigVal, requireConfigVal
    -- *** Access config (advanced, internal)
    , getAppConfigVal, requireAppConfigVal, getConfig, getConfigInternal
    -- ** Types
    , Topic
    -- ** Advanced Actions
    , extractAction, extractReaction
    -- * Internals
    -- ** Values
    , defaultBotName
    -- ** Functions
    , runDefinitions
    -- ** Types
    , BotActionState(BotActionState)
    , BotReacting(..), Script(..), ScriptDefinition(..), ScriptInit(..), ScriptId(..), Handlers(..)
    -- ** Helper lenses
    , HasActions(actions), HasHears(hears), HasResponds(responds), HasJoins(joins), HasCustoms(customs), HasJoinsIn(joinsIn), HasLeaves(leaves), HasLeavesFrom(leavesFrom), HasTopicChange(topicChange), HasTopicChangeIn(topicChangeIn)
    -- ** HelperClasses
    , AccessAdapter(AdapterT, getAdapter), Get(getLens)
    ) where


import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Configurator        as C
import qualified Data.Configurator.Types  as C

import           Control.Arrow
import           Control.Exception.Lifted
import           Control.Lens             hiding (cons)
import           Control.Monad.Logger
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe              (fromMaybe)
import           Data.Monoid             ((<>))
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as L
import           Data.Vector              (Vector)
import qualified Data.Vector              as V
import           Marvin.Adapter           (IsAdapter)
import qualified Marvin.Adapter           as A
import           Marvin.Internal.Types    hiding (getUsername, resolveChannel, messageChannel, getChannelName, resolveChannel)
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex        (Match, Regex)
import           Util


defaultBotName :: L.Text
defaultBotName = "marvin"

-- | Read only data available to a handler when the bot reacts to an event.
declareFields [d|
    data BotActionState a d = BotActionState
        { botActionStateScriptId :: ScriptId
        , botActionStateConfig   :: C.Config
        , botActionStateAdapter :: a
        , botActionStatePayload :: d
        }
    |]


type Topic = L.Text


declareFields [d|
    data Handlers a = Handlers
        { handlersResponds :: Vector (Regex, Match -> Message a -> RunnerM ())
        , handlersHears :: Vector (Regex, Match -> Message a -> RunnerM ())
        , handlersCustoms :: Vector (Event a -> Maybe (RunnerM ()))
        , handlersJoins :: Vector ((User' a, Channel' a) -> RunnerM ())
        , handlersLeaves :: Vector ((User' a, Channel' a) -> RunnerM ())
        , handlersTopicChange :: Vector ((Topic, Channel' a) -> RunnerM ())
        , handlersJoinsIn :: HM.HashMap L.Text (Vector ((User' a, Channel' a) -> RunnerM ()))
        , handlersLeavesFrom :: HM.HashMap L.Text (Vector ((User' a, Channel' a) -> RunnerM ()))
        , handlersTopicChangeIn :: HM.HashMap L.Text (Vector ((Topic, Channel' a) -> RunnerM ()))
        }
    |]


instance Monoid (Handlers a) where
    mempty = Handlers mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend (Handlers r1 h1 c1 j1 l1 t1 ji1 li1 ti1)
            (Handlers r2 h2 c2 j2 l2 t2 ji2 li2 ti2)
        = Handlers (r1 <> r2) (h1 <> h2) (c1 <> c2) (j1 <> j2) (l1 <> l2) (t1 <> t2) (ji1 <> ji2) (li1 <> li2) (ti1 <> ti2)


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
newtype BotReacting a d r = BotReacting { runReaction :: ReaderT (BotActionState a d) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadReader (BotActionState a d), MonadLogger, MonadLoggerIO)

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
newtype ScriptDefinition a r = ScriptDefinition { runScript :: StateT (Script a) RunnerM r } deriving (Monad, MonadIO, Applicative, Functor, MonadLogger)


-- | Initializer for a script. This gets run by the server during startup and creates a 'Script'
newtype ScriptInit a = ScriptInit (ScriptId, a -> C.Config -> RunnerM (Script a))


-- | Class which says that there is a way to get to a 'Message' from this type @m@.
class Get a b where
    getLens :: Lens' a b

instance Get (b, Message a) (Message a) where
    getLens = _2

instance Get (Match, b) Match where
    getLens = _1

instance Get (Topic, a) Topic where
    getLens = _1

idLens :: Lens' a a
idLens = lens id (flip const)

instance Get (b, Channel' a) (Channel' a) where
    getLens = _2

instance Get (b, Message a) (Channel' a) where
    getLens = _2 . lens (Channel' . channel) (\a (Channel' b) -> a {channel = b})

instance Get a a where
    getLens = idLens

instance Get (User' a, b) (User' a) where
    getLens = _1

instance Get (b, Message a) (User' a) where
    getLens = _2 . lens (User' . sender) (\a (User' b) -> a {sender = b})

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


-- | Get the config part for the currect script
getConfig :: HasConfigAccess m => m C.Config
getConfig = getScriptId >>= getSubConfFor


runBotAction :: ShowT t => ScriptId -> C.Config -> a -> Maybe t -> d -> BotReacting a d () -> RunnerM ()
runBotAction scriptId config adapter trigger data_ action = do
    oldLogFn <- askLoggerIO
    catch
        (liftIO $ flip runLoggingT (loggingAddSourcePrefix $(isT "script.#{scriptId}") oldLogFn) $ flip runReaderT actionState $ runReaction action)
        (onScriptExcept scriptId trigger)
  where
    actionState = BotActionState scriptId config adapter data_

prepareAction :: (MonadState (Script a) m, ShowT t) => Maybe t -> BotReacting a d () -> m (d -> RunnerM ())
prepareAction trigger reac = do
    ada <- use adapter
    cfg <- use config
    sid <- use scriptId
    return $ \d -> runBotAction sid cfg ada trigger d reac


onScriptExcept :: ShowT t => ScriptId -> Maybe t -> SomeException -> RunnerM ()
onScriptExcept id trigger e = do
    case trigger of
        Just t ->
            err $(isT "Unhandled exception during execution of script #{id} with trigger #{t}")
        Nothing ->
            err $(isT "Unhandled exception during execution of script #{id}")
    err $(isT "#{e}")
  where
    err = logErrorNS "#{applicationScriptId}.dispatch"

-- | Whenever any message matches the provided regex this handler gets run.
--
-- Equivalent to "robot.hear" in hubot
hear :: Regex -> BotReacting a (Match, Message a) () -> ScriptDefinition a ()
hear !re ac = ScriptDefinition $ do
    pac <- prepareAction (Just re) ac
    actions . hears %= V.cons (re, curry pac)

-- | Runs the handler only if the bot was directly addressed.
--
-- Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting a (Match, Message a) () -> ScriptDefinition a ()
respond !re ac = ScriptDefinition $ do
    pac <- prepareAction (Just re) ac
    actions . responds %= V.cons (re, curry pac)


-- | This handler runs whenever a user enters __any channel__ (which the bot is subscribed to)
--
-- The payload contains the entering user and the channel which was entered.
enter :: BotReacting a (User a, Channel a) () -> ScriptDefinition a ()
enter ac = ScriptDefinition $ do
    pac <- prepareAction (Just "enter event" :: Maybe T.Text) ac
    actions . joins %= V.cons (pac . (unwrapUser' *** unwrapChannel'))


-- | This handler runs whenever a user exits __any channel__ (which the bot is subscribed to)
--
-- The payload contains the exiting user and the channel which was exited.
exit :: BotReacting a (User a, Channel a) () -> ScriptDefinition a ()
exit ac = ScriptDefinition $ do
    pac <- prepareAction (Just "exit event" :: Maybe T.Text) ac
    actions . leaves %= V.cons (pac . (unwrapUser' *** unwrapChannel'))


alterHelper :: a -> Maybe (Vector a) -> Maybe (Vector a)
alterHelper v = return . maybe (return v) (V.cons v)


-- | This handler runs whenever a user enters __the specified channel__.
--
-- The argument is the human readable name for the channel.
--
-- The payload contains the entering user.
enterIn :: L.Text -> BotReacting a (User a, Channel a) () -> ScriptDefinition a ()
enterIn !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "anter event in #{chanName}")) ac
    actions . joinsIn %= HM.alter (alterHelper (pac . (unwrapUser' *** unwrapChannel'))) chanName


-- | This handler runs whenever a user exits __the specified channel__, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable name for the channel.
--
-- The payload contains the exting user.
exitFrom :: L.Text -> BotReacting a (User a, Channel a) () -> ScriptDefinition a ()
exitFrom !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "exit event in #{chanName}")) ac
    actions . leavesFrom %= HM.alter (alterHelper (pac . (unwrapUser' *** unwrapChannel'))) chanName


-- | This handler runs when the topic in __any channel__ the bot is subscribed to changes.
--
-- The payload contains the new topic and the channel in which it was set.
topic :: BotReacting a (Topic, Channel a) () -> ScriptDefinition a ()
topic ac = ScriptDefinition $ do
    pac <- prepareAction (Just "topic event" :: Maybe T.Text) ac
    actions . topicChange %= V.cons (pac . second unwrapChannel')


-- | This handler runs when the topic in __the specified channel__ is changed, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable channel name.
topicIn :: L.Text -> BotReacting a (Topic, Channel a) () -> ScriptDefinition a ()
topicIn !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "topic event in #{chanName}")) ac
    actions . topicChangeIn %= HM.alter (alterHelper (pac . second unwrapChannel')) chanName


-- | Extension point for the user
--
-- Allows you to handle the raw event yourself.
-- Returning 'Nothing' from the trigger function means you dont want to react to the event.
-- The value returned inside the 'Just' is available in the handler later using 'getData'.
customTrigger :: (A.Event a -> Maybe d) -> BotReacting a d () -> ScriptDefinition a ()
customTrigger tr ac = ScriptDefinition $ do
    pac <- prepareAction (Nothing :: Maybe T.Text) ac
    actions . customs %= V.cons (maybe Nothing (return . pac) . tr)


-- | Send a message to the channel the triggering message came from.
--
-- Equivalent to "robot.send" in hubot
send :: (IsAdapter a, Get m (Channel' a)) => L.Text -> BotReacting a m ()
send msg = do
    o <- getChannel
    messageChannel' o msg


-- | Get the username of a registered user.
getUsername :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => User (AdapterT m) -> m L.Text
getUsername usr = do
    a <- getAdapter
    A.liftAdapterAction $ A.getUsername a usr


resolveChannel :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => L.Text -> m (Maybe (Channel (AdapterT m)))
resolveChannel name = do
    a <- getAdapter
    A.liftAdapterAction (A.resolveChannel a name)


-- | Get the human readable name of a channel.
getChannelName :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel (AdapterT m) -> m L.Text
getChannelName rm = do
    a <- getAdapter
    A.liftAdapterAction $ A.getChannelName a rm


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: (IsAdapter a, Get m (Message a)) => L.Text -> BotReacting a m ()
reply msg = do
    om <- getMessage
    user <- getUsername $ sender om
    messageChannel' (channel om) $ user <> " " <> msg


-- | Send a message to a Channel (by name)
messageChannel :: (AccessAdapter m, IsAdapter (AdapterT m), MonadLoggerIO m) => L.Text -> L.Text -> m ()
messageChannel name msg = do
    mchan <- resolveChannel name
    maybe ($logError $(isT "No channel known with the name #{name}")) (`messageChannel'` msg) mchan


messageChannel' :: (AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel (AdapterT m) -> L.Text -> m ()
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
getData = view payload


-- | Get the results from matching the regular expression.
--
-- Equivalent to "msg.match" in hubot.
getMatch :: Get m Match => BotReacting a m Match
getMatch = view (payload . getLens)


-- | Get the message that triggered this action
-- Includes sender, target channel, as well as the full, untruncated text of the original message
getMessage :: Get m (Message a) => BotReacting a m (Message a)
getMessage = view (payload . getLens)


-- | Get the the new topic.
getTopic :: Get m Topic => BotReacting a m Topic
getTopic = view (payload . getLens)


-- | Get the stored channel in which something happened.
getChannel :: forall a m. Get m (Channel' a) => BotReacting a m (Channel a)
getChannel = (unwrapChannel' :: Channel' a -> Channel a) <$> view (payload . getLens)


-- | Get the user whihc was part of the triggered action.
getUser :: forall m a. Get m (User' a) => BotReacting a m (User a)
getUser = (unwrapUser' :: User' a -> User a) <$> view (payload . getLens)


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


-- | INTERNAL, USE WITH CARE
--
-- Get the configuration for the bot (should be "bot" subconfig)
getAppConfig :: HasConfigAccess m => m C.Config
getAppConfig = getSubConfFor applicationScriptId


-- | INTERNAL, USE WITH CARE
--
-- Get a value from the bot config (should be "bot" subconfig)
getAppConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m (Maybe a)
getAppConfigVal name = do
    cfg <- getAppConfig
    liftIO $ C.lookup cfg name


-- | INTERNAL, USE WITH CARE
--
-- Get a value from the bot config (should be "bot" subconfig)
requireAppConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m a
requireAppConfigVal name = do
    cfg <- getAppConfig
    liftIO $ C.require cfg name


-- | Get the configured name of the bot.
getBotName :: HasConfigAccess m => m L.Text
getBotName = fromMaybe defaultBotName <$> getAppConfigVal "name"


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
