{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Marvin.Internal
    (
    -- * Exposed API
    defineScript
    -- ** Reacting
    , hear, respond, topic, topicIn, enter, exit, enterIn, exitFrom, customTrigger
    -- ** Sending messages
    , send, reply, messageChannel, messageChannel'
    -- ** Getting Data
    , getData, getUser, getMatch, getMessage, getChannel, getTopic, getBotName, getChannelName, resolveChannel, getUsername, resolveUser
    -- *** File interactions
    , getFileName, getFileType, getFileUrl, readFileContents, shareLocalFile, getFileSize
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


import           Control.Exception.Lifted
import           Control.Lens              hiding (cons)
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Configurator         as C
import qualified Data.Configurator.Types   as C
import qualified Data.HashMap.Strict       as HM
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as L
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Marvin.Adapter            (IsAdapter)
import qualified Marvin.Adapter            as A
import           Marvin.Internal.Types     hiding (getChannelName, getFileName, getFileSize,
                                            getFileType, getFileUrl, getUsername, messageChannel,
                                            readFileContents, resolveChannel, resolveChannel,
                                            resolveUser, shareLocalFile)
import           Marvin.Internal.Values
import           Marvin.Interpolate.String
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex         (Match, Regex)
import           Util


getSubConfFor :: HasConfigAccess m => ScriptId -> m C.Config
getSubConfFor (ScriptId name) = C.subconfig $(isT "#{scriptConfigKey}.#{name}") <$> getConfigInternal


-- | Get the config part for the currect script
getConfig :: HasConfigAccess m => m C.Config
getConfig = getScriptId >>= getSubConfFor


runBotAction :: ShowT t => ScriptId -> C.Config -> a -> Maybe t -> d -> BotReacting a d () -> RunnerM ()
runBotAction scriptId config adapter trigger data_ action = do
    oldLogFn <- askLoggerIO
    catch
        (liftIO $ flip runLoggingT (loggingAddSourcePrefix $(isT "#{scriptConfigKey}.#{scriptId}") oldLogFn) $ flip runReaderT actionState $ runReaction action)
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
            err $(isT "Unhandled exception during execution of script \"#{id}\" with trigger \"#{t}\"")
        Nothing ->
            err $(isT "Unhandled exception during execution of script \"#{id}\"")
    err $(isT "#{e}")
  where
    err = logErrorNS $(isT "#{applicationScriptId}.dispatch")

-- | Whenever any message matches the provided regex this handler gets run.
--
-- Equivalent to "robot.hear" in hubot
hear :: Regex -> BotReacting a (User' a, Channel' a, Match, Message, TimeStamp a) () -> ScriptDefinition a ()
hear !re ac = ScriptDefinition $ do
    pac <- prepareAction (Just re) ac
    actions . hears %= V.cons (re, pac)

-- | Runs the handler only if the bot was directly addressed.
--
-- Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting a (User' a, Channel' a, Match, Message, TimeStamp a) () -> ScriptDefinition a ()
respond !re ac = ScriptDefinition $ do
    pac <- prepareAction (Just re) ac
    actions . responds %= V.cons (re, pac)


-- | This handler runs whenever a user enters __any channel__ (which the bot is subscribed to)
--
-- The payload contains the entering user and the channel which was entered.
enter :: BotReacting a (User' a, Channel' a, TimeStamp a) () -> ScriptDefinition a ()
enter ac = ScriptDefinition $ do
    pac <- prepareAction (Just "enter event" :: Maybe T.Text) ac
    actions . joins %= V.cons pac


-- | This handler runs whenever a user exits __any channel__ (which the bot is subscribed to)
--
-- The payload contains the exiting user and the channel which was exited.
exit :: BotReacting a (User' a, Channel' a, TimeStamp a) () -> ScriptDefinition a ()
exit ac = ScriptDefinition $ do
    pac <- prepareAction (Just "exit event" :: Maybe T.Text) ac
    actions . leaves %= V.cons pac


alterHelper :: a -> Maybe (Vector a) -> Maybe (Vector a)
alterHelper v = return . maybe (return v) (V.cons v)


-- | This handler runs whenever a user enters __the specified channel__.
--
-- The argument is the human readable name for the channel.
--
-- The payload contains the entering user.
enterIn :: L.Text -> BotReacting a (User' a, Channel' a, TimeStamp a) () -> ScriptDefinition a ()
enterIn !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "enter event in #{chanName}")) ac
    actions . joinsIn %= HM.alter (alterHelper pac) chanName


-- | This handler runs whenever a user exits __the specified channel__, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable name for the channel.
--
-- The payload contains the exting user.
exitFrom :: L.Text -> BotReacting a (User' a, Channel' a, TimeStamp a) () -> ScriptDefinition a ()
exitFrom !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "exit event in #{chanName}")) ac
    actions . leavesFrom %= HM.alter (alterHelper pac) chanName


-- | This handler runs when the topic in __any channel__ the bot is subscribed to changes.
--
-- The payload contains the new topic and the channel in which it was set.
topic :: BotReacting a (User' a, Channel' a, Topic, TimeStamp a) () -> ScriptDefinition a ()
topic ac = ScriptDefinition $ do
    pac <- prepareAction (Just "topic event" :: Maybe T.Text) ac
    actions . topicChange %= V.cons pac


-- | This handler runs when the topic in __the specified channel__ is changed, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable channel name.
topicIn :: L.Text -> BotReacting a (User' a, Channel' a, Topic, TimeStamp a) () -> ScriptDefinition a ()
topicIn !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "topic event in #{chanName}")) ac
    actions . topicChangeIn %= HM.alter (alterHelper pac) chanName


fileShared :: BotReacting a (User' a, Channel' a, File' a, TimeStamp a) () -> ScriptDefinition a ()
fileShared ac = ScriptDefinition $ do
    pac <- prepareAction (Just "file event" :: Maybe T.Text) ac
    actions . fileShares %= V.cons pac


fileSharedIn :: L.Text -> BotReacting a (User' a, Channel' a, File' a, TimeStamp a) () -> ScriptDefinition a ()
fileSharedIn !chanName ac = ScriptDefinition $ do
    pac <- prepareAction (Just $(isT "file event in #{chanName}")) ac
    actions . fileSharesIn %= HM.alter (alterHelper pac) chanName


-- | Extension point for the user
--
-- Allows you to handle the raw event yourself.
-- Returning 'Nothing' from the trigger function means you dont want to react to the event.
-- The value returned inside the 'Just' is available in the handler later using 'getData'.
customTrigger :: (Event a -> Maybe d) -> BotReacting a d () -> ScriptDefinition a ()
customTrigger tr ac = ScriptDefinition $ do
    pac <- prepareAction (Nothing :: Maybe T.Text) ac
    actions . customs %= V.cons (maybe Nothing (return . pac) . tr)


-- | Send a message to the channel the triggering message came from.
--
-- Equivalent to "robot.send" in hubot
send :: (IsAdapter a, Get d (Channel' a)) => L.Text -> BotReacting a d ()
send msg = do
    o <- getChannel
    messageChannel' o msg


-- | Get the username of a registered user. The type signature is so large to allow this function to be used both in 'BotReacting' and 'ScriptDefinition'.
getUsername :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
            => User a -> m L.Text
getUsername = A.liftAdapterAction . A.getUsername


-- | Try to get the channel with a particular human readable name. The type signature is so large to allow this function to be used both in 'BotReacting' and 'ScriptDefinition'.
resolveChannel :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
               => L.Text -> m (Maybe (Channel a))
resolveChannel =  A.liftAdapterAction . A.resolveChannel


-- | Get the human readable name of a channel. The type signature is so large to allow this function to be used both in 'BotReacting' and 'ScriptDefinition'.
getChannelName :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
               => Channel a -> m L.Text
getChannelName = A.liftAdapterAction . A.getChannelName


-- | Get the name of the file, if it has one.
getFileName :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
            => File a -> m (Maybe L.Text)
getFileName = A.liftAdapterAction . A.getFileName


-- | Get the type of the file. For example "text".
getFileType :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
            => File a -> m L.Text
getFileType = A.liftAdapterAction . A.getFileType


-- | A url where this file may be downloaded. Should be at least public enough for the bot to be able to access it.
getFileUrl :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
           => File a -> m (Maybe L.Text)
getFileUrl = A.liftAdapterAction . A.getFileUrl


-- | Return the contents of the file as text. If the file is not public, or cannot be interpreted as text returns 'Nothing'.
readFileContents :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
           => File a -> m (Maybe L.Text)
readFileContents = A.liftAdapterAction . A.readFileContents


-- | Return a timestamp for when this file was created.
getCreationDate :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
                => File a -> m (TimeStamp a)
getCreationDate = A.liftAdapterAction . A.getCreationDate


-- | Return a size of this file in bytes.
getFileSize :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
            => File a -> m Int
getFileSize = A.liftAdapterAction . A.getFileSize


-- | Share a local file to the supplied list of channels
shareLocalFile :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
            => L.Text -> [Channel a] -> m ()
shareLocalFile path = A.liftAdapterAction . A.shareLocalFile path


-- | Try to get the user with a particular username. The type signature is so large to allow this function to be used both in 'BotReacting' and 'ScriptDefinition'.
resolveUser :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
            => L.Text -> m (Maybe (User a))
resolveUser = A.liftAdapterAction . A.resolveUser


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: (IsAdapter a, Get d (User' a), Get d (Channel' a)) => L.Text -> BotReacting a d ()
reply msg = do
    chan <- getChannel
    user <- getUser >>= getUsername
    messageChannel' chan $ user <> " " <> msg


-- | Send a message to a Channel (by name)
messageChannel :: (HasConfigAccess m, AccessAdapter m, IsAdapter (AdapterT m), MonadLoggerIO m) => L.Text -> L.Text -> m ()
messageChannel name msg = do
    mchan <- resolveChannel name
    maybe ($logError $(isT "No channel known with the name #{name}")) (`messageChannel'` msg) mchan


-- | Send a message to a channel (by adapter dependent channel object)
messageChannel' :: (HasConfigAccess m, AccessAdapter m, IsAdapter (AdapterT m), MonadIO m) => Channel (AdapterT m) -> L.Text -> m ()
messageChannel' chan = A.liftAdapterAction . A.messageChannel chan



-- | Define a new script for marvin
--
-- You need to provide a ScriptId (which can be written as a non-empty string, needs the @OverloadedStrings@ language extension).
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
getMessage :: Get m Message => BotReacting a m Message
getMessage = view (payload . getLens)


-- | Get the the new topic.
getTopic :: Get m Topic => BotReacting a m Topic
getTopic = view (payload . getLens)


-- | Get the stored channel in which something happened.
getChannel :: forall a m. Get m (Channel' a) => BotReacting a m (Channel a)
getChannel = (unwrapChannel' :: Channel' a -> Channel a) <$> view (payload . getLens)


-- | Get the user which was part of the triggered action.
getUser :: forall m a. Get m (User' a) => BotReacting a m (User a)
getUser = (unwrapUser' :: User' a -> User a) <$> view (payload . getLens)


-- | Get the stored file.
getFile :: forall a m. Get m (File' a) => BotReacting a m (File a)
getFile = (unwrapFile' :: File' a -> File a) <$> view (payload . getLens)


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
    l <- liftIO $ C.lookup cfg name
    case l of
        Just v -> return v
        _ -> do
            sid <- getScriptId
            error $(isS "Could not find required config value \"#{name}\" in script \"#{sid}\"")


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
extractReaction reac = BotReacting $
    runStderrLoggingT . runReaderT (runReaction reac) <$> ask


-- | Take an action and produce an IO action with the same effect.
-- Useful for creating actions which can be scheduled to execute a certain time or asynchronous.
-- The idea is that one can conveniently send messages from inside a schedulable action.
extractAction :: BotReacting a () o -> ScriptDefinition a (IO o)
extractAction ac = ScriptDefinition $
    fmap (runStderrLoggingT . runReaderT (runReaction ac)) $
        BotActionState <$> use scriptId <*> use config <*> use adapter <*> pure ()


