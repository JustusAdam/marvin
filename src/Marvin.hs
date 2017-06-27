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
    -- | Special interactions for adapters which support the file api 'HasFiles'
    , readTextFile, readFileBytes, newLocalFile, shareFile
    , saveFile, saveFileTo, saveFileToDir
    , sendFile, sendFileTo, sendFileTo'
    -- ** Interaction with the config
    , getConfigVal, requireConfigVal, getBotName
    -- ** Handler Types
    , BotReacting
    , Message, Topic, FileContent(..)
    -- * Utility functions
    , textToPath, pathToText
    -- * The Script
    , Script, defineScript, ScriptInit
    , ScriptId
    , ScriptDefinition
    -- * The adapter
    , IsAdapter(User, Channel), HasFiles(LocalFile, RemoteFile), MonadAdapter(AdapterT, liftAdapterM)
    -- * Lenses
    , HasUsername(username), HasName(name), HasFirstName(firstName), HasLastName(lastName), HasFileType(fileType), HasUrl(url), HasCreationDate(creationDate), HasSize(size), HasContent(content)
    -- * Advanced actions
    , extractAction, extractReaction
    ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader        (ask, runReaderT)
import           Control.Monad.State         (MonadState)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as B
import qualified Data.HashMap.Strict         as HM
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as L
import           Data.Time.Clock
import qualified Data.Vector                 as V
import           Lens.Micro.Platform
import           Marvin.Adapter              (HasFiles(LocalFile, RemoteFile), IsAdapter)
import qualified Marvin.Adapter              as A
import           Marvin.Internal
import           Marvin.Internal.LensClasses
import           Marvin.Internal.Types       (BotActionState(BotActionState),
                                              BotReacting(BotReacting, runReaction),
                                              HasConfigAccess, MonadAdapter(AdapterT),
                                              ScriptDefinition(ScriptDefinition),
                                              ScriptInit(ScriptInit))
import           Marvin.Internal.Values      (defaultBotName)
import           Marvin.Interpolate.All
import           Marvin.Types
import qualified Marvin.Util.Config          as C
import           Marvin.Util.Regex           (Match, Regex)
import           System.Directory
import           System.FilePath


prepareAction :: (MonadState (Script a) m, ShowT t) => Maybe t -> BotReacting a d () -> m (d -> RunnerM ())
prepareAction trigger reac = do
    f <- runBotAction <$> use scriptId <*> use config <*> use adapter
    return $ \d -> f trigger d reac


-- | Whenever any message matches the provided regex this handler gets run.
--
-- Equivalent to "robot.hear" in hubot
hear :: Regex -> BotReacting a (User' a, Channel' a, Match, Message, TimeStamp a) () -> ScriptDefinition a ()
hear !regex ac = ScriptDefinition $ do
    pac <- prepareAction (Just regex) ac
    actions . hears %= V.cons (regex, pac)

-- | Runs the handler only if the bot was directly addressed.
--
-- Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting a (User' a, Channel' a, Match, Message, TimeStamp a) () -> ScriptDefinition a ()
respond !regex ac = ScriptDefinition $ do
    pac <- prepareAction (Just regex) ac
    actions . responds %= V.cons (regex, pac)


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
send msg = flip messageChannel' msg =<< getChannel


-- | Try to get the channel with a particular human readable name.
resolveChannel :: MonadAdapter m
               => L.Text -> m (Maybe (Channel (AdapterT m)))
resolveChannel =  liftAdapterM . A.resolveChannel


-- | Return the contents of the file as text. If the file is not public, or cannot be interpreted as text returns 'Nothing'.
readTextFile :: (MonadAdapter m, HasFiles (AdapterT m))
           => RemoteFile (AdapterT m) -> m (Maybe L.Text)
readTextFile = liftAdapterM . A.readTextFile


-- | Attempt to download the remote file into the current directory.
--
-- Uses either the name of the downloaded file as filename or @"unnamed-\<current time\>"@
--
-- When successful returns the path of the saved file otherwise an error message.
saveFile :: (MonadAdapter m, MonadIO m, HasFiles (AdapterT m))
         => RemoteFile (AdapterT m) -> m (Either L.Text FilePath)
saveFile = (`saveFileToDir` ".")


-- | Attempt to download the remote file into the specified directory.
--
-- Uses either the name of the downloaded file as filename or @"unnamed-\<current time\>"@
--
-- When successful returns the path of the saved file otherwise an error message.
saveFileToDir :: (MonadAdapter m, MonadIO m, HasFiles (AdapterT m))
              => RemoteFile (AdapterT m) -> FilePath -> m (Either L.Text FilePath)
saveFileToDir file dir = do
    fname <- liftIO $
        case file^.name of
            Nothing -> do
                ts <- getCurrentTime
                return $(isS "unnamed-#{ts}")
            Just n -> return $ L.unpack n
    saveFileTo file (dir </> fname)


-- | Attempt to download the remote file to the designated path.
--
-- Uses either the name of the downloaded file as filename or @"unnamed-\<current time\>"@
-- Creates intermediate directories if they are missing.
--
-- When successful returns the path of the saved file otherwise an error message.
saveFileTo :: (MonadAdapter m, MonadIO m, HasFiles (AdapterT m))
           => RemoteFile (AdapterT m) -> FilePath -> m (Either L.Text FilePath)
saveFileTo file path = readFileBytes file >>= \case
    Nothing ->
        return $ Left $(isL "#{maybe \"unnamed file\" (\"File \" <>) $ file^.name} could not be downloaded")
    Just text -> do
        liftIO $ do
            createDirectoryIfMissing True $ takeDirectory path
            B.writeFile path text
        return $ return path


-- | Share the file with the provided path to the channel we are currently responding to.
sendFile :: (IsAdapter a, HasFiles a, Get m (Channel' a)) => FilePath -> BotReacting a m (Either L.Text (RemoteFile a))
sendFile path = sendFileTo' path . return =<< getChannel


-- | Share the file with the provided path to the channel with the specified name.
sendFileTo :: (MonadAdapter m, AdapterT m ~ a, HasFiles a, MonadIO m) => FilePath -> L.Text -> m (Either L.Text (RemoteFile a))
sendFileTo path chanName = maybe (return $ Left "Channel does not exist") (sendFileTo' path . return) =<< resolveChannel chanName


-- | Share the file with the provided path to the channel.
sendFileTo' :: (MonadAdapter m, HasFiles a, MonadIO m, AdapterT m ~ a) => FilePath -> [Channel a] -> m (Either L.Text (RemoteFile a))
sendFileTo' path chans = runExceptT $ do
    e <- liftIO $ doesFileExist path
    when e $ fail "File does not exist"
    file <- lift $ newLocalFile path' (FileOnDisk path)
    ExceptT $ shareFile file chans
  where path' = L.pack path


-- | Return the contents of the file as bytes. If the file is not public returns 'Nothing'.
readFileBytes :: (MonadAdapter m, HasFiles (AdapterT m))
           => RemoteFile (AdapterT m) -> m (Maybe ByteString)
readFileBytes = liftAdapterM . A.readFileBytes


-- | Create a new 'LocalFile' object for upload with the specified content.
newLocalFile :: (MonadAdapter m, HasFiles (AdapterT m))
            => L.Text -> FileContent -> m (LocalFile (AdapterT m))
newLocalFile fname = liftAdapterM . A.newLocalFile fname


-- | Share a local file to the supplied list of channels
shareFile :: (MonadAdapter m, HasFiles a, AdapterT m ~ a)
            => LocalFile a -> [Channel a] -> m (Either L.Text (RemoteFile a))
shareFile f = liftAdapterM . A.shareFile f


-- | Try to get the user with a particular username.
resolveUser :: MonadAdapter m
            => L.Text -> m (Maybe (User (AdapterT m)))
resolveUser = liftAdapterM . A.resolveUser


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: (IsAdapter a, Get d (User' a), Get d (Channel' a)) => L.Text -> BotReacting a d ()
reply msg = do
    chan <- getChannel
    user <- (^.username) <$> getUser
    messageChannel' chan $ user <> " " <> msg


-- | Send a message to a Channel (by name)
messageChannel :: (MonadAdapter m, MonadLogger m) => L.Text -> L.Text -> m ()
messageChannel fname msg = maybe ($logError $(isT "No channel known with the name #{fname}")) (`messageChannel'` msg) =<< resolveChannel fname


-- | Send a message to a channel (by adapter dependent channel object)
messageChannel' :: MonadAdapter m => Channel (AdapterT m) -> L.Text -> m ()
messageChannel' chan = liftAdapterM . A.messageChannel chan



-- | Define a new script for marvin
--
-- You need to provide a ScriptId (which can be written as a non-empty string, needs the @OverloadedStrings@ language extension).
-- This id is used as the key for the section in the bot config belonging to this script and in logging output.
--
-- Roughly equivalent to "module.exports" in hubot.
defineScript :: ScriptId -> ScriptDefinition a () -> ScriptInit a
defineScript sid definitions = ScriptInit (sid, runDefinitions sid definitions)


-- | Obtain the event reaction data.
--
-- The type of this data depends on the reaction function used.
-- Generally this will be a tuple of various information about the current event.
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
getUsername :: MonadAdapter m
            => User (AdapterT m)
            -> m L.Text
getUsername u = pure $ u^.username
{-# DEPRECATED getUsername "Will be remove in version 1.0, use the lens 'username' instead." #-}


-- | Get the human readable name of a channel.
--
-- This function is deprecated as of Version 0.3 and will be removed in version 1.0 use the lens 'name' instead.
getChannelName :: MonadAdapter m
               => Channel (AdapterT m) -> m L.Text
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
getConfigVal key = do
    cfg <- getConfig
    C.lookup cfg key


-- | Get a value out of the config and fail with an error if the specified key is not found.
--
-- This config is the config for this script. Ergo all config vars registered under the config section for the ScriptId of this script.
--
-- The 'HasConfigAccess' Constraint means this function can be used both during script definition and when a handler is run.
requireConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m a
requireConfigVal key = do
    cfg <- getConfig
    C.lookup cfg key >>= \case
        Just v -> return v
        _ -> do
            sid <- getScriptId
            error $(isS "Could not find required config value \"#{key}\" in script \"#{sid}\"")


-- | Get the configured name of the bot.
getBotName :: HasConfigAccess m => m L.Text
getBotName = fromMaybe defaultBotName <$> getAppConfigVal "name"


-- | Take a reaction and produce an IO action with the same effect.
-- Useful for creating actions which can be scheduled to execute a certain time or asynchronous.
-- The idea is that one can conveniently send messages from inside a schedulable action.
extractReaction :: BotReacting a s o -> BotReacting a s (IO o)
extractReaction reac = do
    logger <- askLoggerIO
    BotReacting $ flip runLoggingT logger . runReaderT (runReaction reac) <$> ask


-- | Take an action and produce an IO action with the same effect.
-- Useful for creating actions which can be scheduled to execute a certain time or asynchronous.
-- The idea is that one can conveniently send messages from inside a schedulable action.
extractAction :: BotReacting a () o -> ScriptDefinition a (IO o)
extractAction ac = do
    logger <- askLoggerIO
    ScriptDefinition $
        fmap (flip runLoggingT logger . runReaderT (runReaction ac)) $
            BotActionState <$> use scriptId <*> use config <*> use adapter <*> pure ()


textToPath :: L.Text -> FilePath
textToPath = L.unpack


pathToText :: FilePath -> L.Text
pathToText = L.pack
