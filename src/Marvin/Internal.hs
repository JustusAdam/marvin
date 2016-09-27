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


newtype Regex = Regex { unwrapRegex :: Re.Regex }


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


type Match = [Text]
data BotAnswerState = BotAnswerState
    { botAnswerStateMessage  :: Message
    , botAnswerStateScriptId :: ScriptId
    , botAnswerStateMatch    :: Match
    , botAnswerStateConfig   :: C.Config
    }


newtype BotReacting a = BotReacting { runReaction :: StateT BotAnswerState IO a } deriving (Monad, MonadIO, Applicative, Functor)


data Script = Script
    { scriptReactions :: [(Regex, BotReacting ())]
    , scriptListens   :: [(Regex, BotReacting ())]
    , scriptScriptId  :: ScriptId
    , scriptConfig    :: C.Config
    }



newtype ScriptDefinition a = ScriptDefinition { runScript :: StateT Script IO a } deriving (Monad, MonadIO, Applicative, Functor)


newtype ScriptInit = ScriptInit (C.Config -> IO Script)


class IsScript m where
    getScriptId :: m ScriptId


instance IsScript ScriptDefinition where
    getScriptId = ScriptDefinition $ gets scriptScriptId

instance IsScript BotReacting where
    getScriptId = BotReacting $ gets botAnswerStateScriptId


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


-- | Equivalent to "robot.hear" in hubot
hear :: Regex -> BotReacting () -> ScriptDefinition ()
hear re hn = ScriptDefinition $ modify (\s@(Script {scriptReactions=r}) -> s{ scriptReactions = r ++ return (re, hn) })


-- | Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting () -> ScriptDefinition ()
respond re hn = ScriptDefinition $ modify (\s@(Script {scriptListens=l}) -> s { scriptListens = l ++ return (re, hn) })


-- | Equivalent to "robot.send" in hubot
send :: Text -> BotReacting ()
send msg = do
    o <- getMessage
    messageRoom (channel o) msg


-- | Equivalent to "robot.reply" in hubot
reply :: Text -> BotReacting ()
reply msg = do
    om <- getMessage
    send $ (username $ sender om) ++ " " ++ msg


messageRoom :: Room -> Text -> BotReacting ()
messageRoom room msg = do
    token <- requireAppConfigVal "token"
    liftIO $ async $ post "https://slack.com/api/chat.postMessage"
                        [ "token" := (token :: Text)
                        , "channel" := roomname room
                        , "text" := msg
                        ]
    return ()


-- | Equivalent to "module.exports" in hubot
defineScript :: ScriptId -> ScriptDefinition () -> ScriptInit
defineScript sid definitions =
    ScriptInit $ runDefinitions sid definitions


runDefinitions :: ScriptId -> ScriptDefinition () -> C.Config -> IO Script
runDefinitions sid definitions cfg = execStateT (runScript definitions) (Script mempty mempty sid cfg)


-- | Equivalent to "msg.match" in hubot
getMatch :: BotReacting Match
getMatch = BotReacting $ gets botAnswerStateMatch


-- | Get the message that triggered this action
getMessage :: BotReacting Message
getMessage = BotReacting $ gets botAnswerStateMessage


-- | Get a value out of the config
getConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m (Maybe a)
getConfigVal name = do
    cfg <- getConfig
    liftIO $ C.lookup cfg name


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
r :: [Re.MatchOption] -> Text -> Regex
r opts s = Regex $ Re.regex opts s


instance IsString Regex where
    fromString = r [] . pack


match :: Regex -> Text -> Maybe Match
match r = fmap (Re.unfold Re.group) . Re.find (unwrapRegex r)
