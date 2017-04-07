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
import qualified Data.Configurator        as C
import qualified Data.Configurator.Types  as C
import           Marvin.Internal.Types
import           Marvin.Internal.Values
import           Marvin.Interpolate.Text
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


