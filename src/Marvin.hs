{-|
Module      : $Header$
Description : Marvin the modular bot
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

For the proper, verbose documentation see <https://marvin.readthedocs.org/en/latest/scripting.html>.

-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Marvin
    (
    -- * Reacting
    -- ** Reaction Functions
      hear, respond, enter, exit, enterIn, exitFrom, topic, topicIn, fileShared, fileSharedIn, customTrigger
    -- ** Getting data
    -- | The type signature for the functions in this section is so large to allow this function to be used both in 'BotReacting' and 'ScriptDefinition'.
    , getData, getMessage, getMatch, getTopic, getChannel, getUser, getRemoteFile, getTimeStamp, resolveUser, resolveChannel, getUsername, getChannelName
    -- ** Sending messages
    , send, reply, messageChannel, messageChannel'
    -- ** File interactions
    -- | Special interactions for adapters which support the file api 'HasFiles's
    , readTextFile, readFileBytes, newLocalFile, shareFile
    , saveFile, saveFileTo, saveFileToDir
    -- ** Interaction with the config
    , getConfigVal, requireConfigVal, getBotName
    -- ** Handler Types
    , BotReacting
    , Message, User, Channel, Topic, FileContent(..)
    -- * The Script
    , Script, defineScript, ScriptInit
    , ScriptId
    , ScriptDefinition, IsAdapter, HasFiles
    -- * Lenses
    , HasActions(actions), HasUsername(username), HasName(name), HasFirstName(firstName), HasLastName(lastName), HasFileType(fileType), HasUrl(url), HasCreationDate(creationDate), HasSize(size), HasContent(content)
    -- * Advanced actions
    , extractAction, extractReaction
    ) where


import           Control.Lens
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader        (ask, runReaderT)
import           Control.Monad.State         (MonadState)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as B
import qualified Data.Configurator           as C
import qualified Data.Configurator.Types     as C
import qualified Data.HashMap.Strict         as HM
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Time.Clock
import qualified Data.Vector                 as V
import           Marvin.Adapter              (HasFiles(LocalFile, RemoteFile), IsAdapter)
import qualified Marvin.Adapter              as A
import           Marvin.Internal
import           Marvin.Internal.LensClasses
import           Marvin.Internal.Types       (BotActionState(BotActionState),
                                              BotReacting(BotReacting, runReaction),
                                              ScriptDefinition(ScriptDefinition),
                                              ScriptInit(ScriptInit))
import           Marvin.Internal.Values      (defaultBotName)
import           Marvin.Interpolate.All
import           Marvin.Types
import           Marvin.Util.Regex           (Match, Regex)
import           System.Directory
import           System.FilePath


prepareAction :: (MonadState (Script a) m, ShowT t) => Maybe t -> BotReacting a d () -> m (d -> RunnerM ())
prepareAction trigger reac = do
    ada <- use adapter
    cfg <- use config
    sid <- use scriptId
    return $ \d -> runBotAction sid cfg ada trigger d reac


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


alterHelper :: a -> Maybe (V.Vector a) -> Maybe (V.Vector a)
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


-- | This handler runs when a file is shared in __any channel__ the bot is subscribed to.
--
-- The payload contains information about the file and the channel to which it was shared.
fileShared :: BotReacting a (User' a, Channel' a, RemoteFile' a, TimeStamp a) () -> ScriptDefinition a ()
fileShared ac = ScriptDefinition $ do
    pac <- prepareAction (Just "file event" :: Maybe T.Text) ac
    actions . fileShares %= V.cons pac


-- | This handler runs when a file is shared in __the specified channel__ is changed, provided the bot is subscribed to the channel in question.
--
-- The argument is the human readable channel name.
fileSharedIn :: L.Text -> BotReacting a (User' a, Channel' a, RemoteFile' a, TimeStamp a) () -> ScriptDefinition a ()
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


-- | Try to get the channel with a particular human readable name.
resolveChannel :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
               => L.Text -> m (Maybe (Channel a))
resolveChannel =  A.liftAdapterAction . A.resolveChannel


-- | Return the contents of the file as text. If the file is not public, or cannot be interpreted as text returns 'Nothing'.
readTextFile :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
           => RemoteFile a -> m (Maybe L.Text)
readTextFile = A.liftAdapterAction . A.readTextFile


-- | Attempt to download the remote file into the current directory.
--
-- Uses either the name of the downloaded file as filename or @"unnamed-<current time>"@
--
-- When successful returns the path of the saved file otherwise an error message.
saveFile :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a, MonadLogger m)
         => RemoteFile a -> m (Either L.Text FilePath)
saveFile = (`saveFileToDir` ".")


-- | Attempt to download the remote file into the specified directory.
--
-- Uses either the name of the downloaded file as filename or @"unnamed-<current time>"@
--
-- When successful returns the path of the saved file otherwise an error message.
saveFileToDir :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a, MonadLogger m)
              => RemoteFile a -> FilePath -> m (Either L.Text FilePath)
saveFileToDir file dir = do
    name <- liftIO $
        case file^.name of
            Nothing -> do
                ts <- getCurrentTime
                return $(isS "unnamed-#{ts}")
            Just n -> return $ L.unpack n
    saveFileTo file (dir </> name)


-- | Attempt to download the remote file to the designated path.
--
-- Uses either the name of the downloaded file as filename or @"unnamed-<current time>"@
-- Creates intermediate directories if they are missing.
--
-- When successful returns the path of the saved file otherwise an error message.
saveFileTo :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a, MonadLogger m)
           => RemoteFile a -> FilePath -> m (Either L.Text FilePath)
saveFileTo file path =
    readFileBytes file >>= \case
        Nothing ->
            return $ Left $(isL "#{maybe \"unnamed file\" (\"File \" <>) $ file^.name} could not be downloaded")
        Just text -> do
            liftIO $ do
                createDirectoryIfMissing True $ takeDirectory path
                B.writeFile path text
            return $ return path


-- | Return the contents of the file as bytes. If the file is not public returns 'Nothing'.
readFileBytes :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
           => RemoteFile a -> m (Maybe ByteString)
readFileBytes = A.liftAdapterAction . A.readFileBytes


-- | Create a new 'LocalFile' object for upload with the specified content.
newLocalFile :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
            => L.Text -> FileContent -> m (LocalFile a)
newLocalFile name = A.liftAdapterAction . A.newLocalFile name


-- | Share a local file to the supplied list of channels
shareFile :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, HasFiles a, MonadIO m, AdapterT m ~ a)
            => LocalFile a -> [Channel a] -> m (Either L.Text (RemoteFile a))
shareFile f = A.liftAdapterAction . A.shareFile f


-- | Try to get the user with a particular username.
resolveUser :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
            => L.Text -> m (Maybe (User a))
resolveUser = A.liftAdapterAction . A.resolveUser


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: (IsAdapter a, Get d (User' a), Get d (Channel' a)) => L.Text -> BotReacting a d ()
reply msg = do
    chan <- getChannel
    user <- (^.username) <$> getUser
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

-- | Get the timestamp for when an event took place
getTimeStamp :: forall m a. Get m (TimeStamp a) => BotReacting a m (TimeStamp a)
getTimeStamp = view (payload . getLens)

-- | Get the username of a registered user.
--
-- This function is deprecated as of version 0.3 and will be removed in version 1.0 use the lens 'username' instead.
getUsername :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
            => User a
            -> m L.Text
getUsername u = pure $ u^.username
{-# DEPRECATED getUsername "Will be remove in version 1.0, use the lens 'username' instead." #-}


-- | Get the human readable name of a channel.
--
-- This function is deprecated as of Version 0.3 and will be removed in version 1.0 use the lens 'name' instead.
getChannelName :: (HasConfigAccess m, AccessAdapter m, IsAdapter a, MonadIO m, AdapterT m ~ a)
               => Channel a -> m L.Text
getChannelName c = pure $ c^.name
{-# DEPRECATED getChannelName "Will be remove in version 1.0, use the lens 'name' instead." #-}

-- | Get the stored information about a remote file.
getRemoteFile :: forall a m. Get m (RemoteFile' a) => BotReacting a m (RemoteFile a)
getRemoteFile = (unwrapFile' :: RemoteFile' a -> RemoteFile a) <$> view (payload . getLens)


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

