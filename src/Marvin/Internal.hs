{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Marvin.Internal where


import           Control.Exception.Lifted
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Marvin.Internal.Types
import           Marvin.Internal.Values
import           Marvin.Interpolate.Text
import qualified Marvin.Util.Config       as C
import           Util


getSubConfFor :: HasConfigAccess m => ScriptId -> m C.Config
getSubConfFor (ScriptId name) = C.subconfig $(isT "#{scriptConfigKey}.#{name}") =<< getConfigInternal


-- | Get the config part for the currect script
getConfig :: HasConfigAccess m => m C.Config
getConfig = getScriptId >>= getSubConfFor


runBotAction :: ShowT t => ScriptId -> C.Config -> a -> Maybe t -> d -> BotReacting a d () -> RunnerM ()
runBotAction scriptId config adapter trigger data_ action = do
    oldLogFn <- askLoggerIO

    runScript oldLogFn `catch` onScriptExcept scriptId trigger
  where
    runScript oldLogFn = liftIO
        $ flip runLoggingT (loggingAddSourcePrefix $(isT "#{scriptConfigKey}.#{scriptId}") oldLogFn)
        $ runReaderT (runReaction action) actionState
    actionState = BotActionState scriptId config adapter data_



onScriptExcept :: ShowT t => ScriptId -> Maybe t -> SomeException -> RunnerM ()
onScriptExcept sid trigger e =
    logErrorNS $(isT "#{applicationScriptId}.dispatch") $ case trigger of
        Just t -> $(isT "Unhandled exception during execution of script \"#{sid}\" with trigger \"#{t}\": #{e}")
        Nothing -> $(isT "Unhandled exception during execution of script \"#{sid}\": #{e}")


runDefinitions :: ScriptId -> ScriptDefinition a () -> a -> C.Config -> RunnerM (Script a)
runDefinitions sid definitions ada cfg = execStateT (runScript definitions) (Script mempty sid cfg ada)



-- | INTERNAL, USE WITH CARE
--
-- Get the configuration for the bot (should be "bot" subconfig)
getAppConfig :: HasConfigAccess m => m C.Config
getAppConfig = getSubConfFor applicationScriptId


-- | INTERNAL, USE WITH CARE
--
-- Get a value from the bot config (should be "bot" subconfig)
getAppConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m (Maybe a)
getAppConfigVal name = getAppConfig >>= liftIO . flip C.lookup name


-- | INTERNAL, USE WITH CARE
--
-- Get a value from the bot config (should be "bot" subconfig)
requireAppConfigVal :: (C.Configured a, HasConfigAccess m) => C.Name -> m a
requireAppConfigVal name = getAppConfig >>= liftIO . flip C.require name


