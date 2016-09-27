{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Marvin.Internal where


import           ClassyPrelude
import           Control.Monad.State
import           Data.Char
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import qualified Data.Text.ICU           as Re
import           Marvin.Types
import           Network.Wreq


-- | Abstract Wrapper for a reglar expression implementation. Has an 'IsString' implementation, so literal strings can be used to create a 'Regex'.
-- Alternatively use 'r' to create one with custom options.
newtype Regex = Regex { unwrapRegex :: Re.Regex }


-- | A type, basically a String, which identifies a script to the config and the logging facilities.
newtype ScriptId = ScriptId { unwrapScriptId :: Text } deriving (Show, Eq)


applicationScriptId :: ScriptId
applicationScriptId = ScriptId "bot"


instance IsString ScriptId where
    fromString "" = error "script id must not be empty"
    fromString "bot" = error "'bot' is a protected name and cannot be used as script id"
    fromString s@(x:xs) =
        if isLetter x && all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs
            then ScriptId $ fromString s
            else error "first character of script id must be a letter, all other characters can be alphanumeric, '-' or '_'"


-- | A match to a 'Regex'. Index 0 is the full match, all other indexes are match groups.
type Match = [Text]


data BotAnswerState = BotAnswerState
    { botAnswerStateMessage  :: Message
    , botAnswerStateScriptId :: ScriptId
    , botAnswerStateMatch    :: Match
    , botAnswerStateConfig   :: C.Config
    }


-- | Monad for reacting in the bot. Allows use of functions like 'send', 'reply' and 'messageRoom' as well as any arbitrary 'IO' action.
newtype BotReacting a = BotReacting { runReaction :: StateT BotAnswerState IO a } deriving (Monad, MonadIO, Applicative, Functor)


-- | An abstract type describing a marvin script.
--
-- This is basically a collection of event handlers.
data Script = Script
    { scriptReactions :: [(Regex, BotReacting ())]
    , scriptListens   :: [(Regex, BotReacting ())]
    , scriptScriptId  :: ScriptId
    , scriptConfig    :: C.Config
    }


-- | A monad for gradually defining a 'Script' using 'respond' and 'hear' as well as any 'IO' action.
newtype ScriptDefinition a = ScriptDefinition { runScript :: StateT Script IO a } deriving (Monad, MonadIO, Applicative, Functor)


-- | Initializer for a script. This gets run by the server during startup and creates a 'Script'
newtype ScriptInit = ScriptInit (C.Config -> IO Script)


class IsScript m where
    getScriptId :: m ScriptId


instance IsScript ScriptDefinition where
    getScriptId = ScriptDefinition $ gets scriptScriptId

instance IsScript BotReacting where
    getScriptId = BotReacting $ gets botAnswerStateScriptId


-- | Denotes a place from which we may access the configuration.
--
-- During script definition or when handling a request we can obtain the config with 'getConfigVal' or 'requireConfigVal'.
class (IsScript m, MonadIO m) => HasConfigAccess m where
    getConfigInternal :: m C.Config

instance HasConfigAccess ScriptDefinition where
    getConfigInternal = ScriptDefinition $ gets scriptConfig

instance HasConfigAccess BotReacting where
    getConfigInternal = BotReacting $ gets botAnswerStateConfig


getSubConfFor :: HasConfigAccess m => ScriptId -> m C.Config
getSubConfFor (ScriptId name) = C.subconfig name <$> getConfigInternal


getConfig :: HasConfigAccess m => m C.Config
getConfig = getScriptId >>= getSubConfFor


-- | Whenever any message matches the provided regex this handler gets run.
--
-- Equivalent to "robot.hear" in hubot
hear :: Regex -> BotReacting () -> ScriptDefinition ()
hear re hn = ScriptDefinition $ modify (\s@(Script {scriptReactions=r}) -> s{ scriptReactions = r ++ return (re, hn) })


-- | Runs the handler only if the bot was directly addressed.
--
-- Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting () -> ScriptDefinition ()
respond re hn = ScriptDefinition $ modify (\s@(Script {scriptListens=l}) -> s { scriptListens = l ++ return (re, hn) })


-- | Send a message to the channel the triggering message came from.
--
-- Equivalent to "robot.send" in hubot
send :: Text -> BotReacting ()
send msg = do
    o <- getMessage
    messageRoom (channel o) msg


-- | Send a message to the channel the original message came from and address the user that sent the original message.
--
-- Equivalent to "robot.reply" in hubot
reply :: Text -> BotReacting ()
reply msg = do
    om <- getMessage
    send $ (username $ sender om) ++ " " ++ msg


-- | Send a message to a room
messageRoom :: Room -> Text -> BotReacting ()
messageRoom room msg = do
    token <- requireAppConfigVal "token"
    liftIO $ async $ post "https://slack.com/api/chat.postMessage"
                        [ "token" := (token :: Text)
                        , "channel" := roomname room
                        , "text" := msg
                        ]
    return ()


-- | Define a new script for marvin
--
-- You need to provide a ScriptId (which can simple be written as a non-empty string).
-- This id is used as the key for the section in the bot config belonging to this script and in logging output.
--
-- Roughly equivalent to "module.exports" in hubot.
defineScript :: ScriptId -> ScriptDefinition () -> ScriptInit
defineScript sid definitions =
    ScriptInit $ runDefinitions sid definitions


runDefinitions :: ScriptId -> ScriptDefinition () -> C.Config -> IO Script
runDefinitions sid definitions cfg = execStateT (runScript definitions) (Script mempty mempty sid cfg)


-- | Get the results from matching the regular expression.
--
-- Equivalent to "msg.match" in hubot.
getMatch :: BotReacting Match
getMatch = BotReacting $ gets botAnswerStateMatch


-- | Get the message that triggered this action
-- Includes sender, target channel, as well as the full, untruncated text of the original message
getMessage :: BotReacting Message
getMessage = BotReacting $ gets botAnswerStateMessage


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



-- | Compile a regex with options
--
-- Normally it is sufficient to just write the regex as a plain string and have it be converted automatically, but if you wnat certain match options you can use this function.
r :: [Re.MatchOption] -> Text -> Regex
r opts s = Regex $ Re.regex opts s


instance IsString Regex where
    fromString = r [] . pack


-- | Match a regex against a string and return the first match found (if any).
match :: Regex -> Text -> Maybe Match
match r = fmap (Re.unfold Re.group) . Re.find (unwrapRegex r)
