{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Framework.SlackBot.Internal where


import ClassyPrelude
import Framework.SlackBot.Types
import qualified Text.Regex.PCRE.Light as PCRE
import Data.String
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Char
import Control.Monad.State
import qualified Data.ByteString.Char8 as C


newtype Regex = Regex { unwrapRegex :: PCRE.Regex } 


newtype ScriptId = ScriptId Text deriving (Show, Eq)


instance IsString ScriptId where
    fromString "" = error "script id must not be empty"
    fromString "application" = error "'application' is a protected name and cannot be used as script id"
    fromString s@(x:xs) =
        if isLetter x && all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs
            then ScriptId $ fromString s
            else error "first character of script id must be a letter, all other characters can be alphanumeric, '-' or '_'"


type Match = [Text]
data BotAnswerState = BotAnswerState
    { botAnswerStateMessage :: Message
    , botAnswerStateScriptId :: ScriptId
    , botAnswerStateMatch :: Match
    , botAnswerStateConfig :: C.Config
    }


newtype BotReacting a = BotReacting { runReaction :: StateT BotAnswerState IO a } deriving (Monad, MonadIO, Applicative, Functor)


data Script = Script 
    { scriptReactions :: Seq (Regex, BotReacting ())
    , scriptListens :: Seq (Regex, BotReacting ())
    , scriptScriptId :: ScriptId
    , scriptConfig :: C.Config
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
    getConfig' :: m C.Config

instance HasConfigAccess ScriptDefinition where
    getConfig' = ScriptDefinition $ gets scriptConfig

instance HasConfigAccess BotReacting where
    getConfig' = BotReacting $ gets botAnswerStateConfig

getConfig :: HasConfigAccess m => m C.Config
getConfig = do
    (ScriptId sub) <- getScriptId
    cfg <- getConfig
    return $ C.subconfig sub cfg


-- | Equivalent to "robot.hear" in hubot
react :: Regex -> BotReacting () -> ScriptDefinition ()
react re hn = ScriptDefinition $ modify (\s@(Script {scriptReactions=r}) -> s{ scriptReactions = r ++ return (re, hn) })


-- | Equivalent to "robot.respond" in hubot
respond :: Regex -> BotReacting () -> ScriptDefinition ()
respond re hn = ScriptDefinition $ modify (\s@(Script {scriptListens=l}) -> s { scriptListens = l ++ return (re, hn) })


-- | Equivalent to "robot.send" in hubot
send :: Text -> BotReacting ()
send msg = do 
    o <- getMessage
    messageRoom (channel o) msg


messageRoom :: Room -> Text -> BotReacting ()
messageRoom room msg = undefined


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


-- | Compile a regex with options
r :: ByteString -> [PCRE.PCREOption] -> Regex
r s opts = Regex $ PCRE.compile s opts


instance IsString Regex where
    fromString s = r (C.pack s) []
