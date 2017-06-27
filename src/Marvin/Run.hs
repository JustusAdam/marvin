{-|
Module      : $Header$
Description : Running marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Marvin.Run
    ( runMarvin, ScriptInit, IsAdapter
    , requireFromAppConfig, lookupFromAppConfig, defaultConfigName
    -- * Rolling your own
    , runMarvinWithConfig, parseMarvinCmdArgs, marvinCmdArgsParser
    , setLoggingLevelIn, runStderrLoggingT, getLoggingLevel, CmdOptions, configPath, verbose, debug
    ) where


import           Control.Concurrent.Async.Lifted (async, link, mapConcurrently_, wait)
import           Control.Concurrent.Chan.Lifted
import           Control.DeepSeq
import           Control.Exception.Lifted
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Configurator               as Cfg
import qualified Data.HashMap.Strict             as HM
import           Data.Maybe                      (catMaybes, fromJust, fromMaybe, isJust)
import           Data.Monoid                     ((<>))
import qualified Data.Text.Lazy                  as L
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           Lens.Micro.Platform             hiding (cons)
import           Marvin.Internal.LensClasses
import           Marvin.Internal.Types
import           Marvin.Internal.Values
import           Marvin.Interpolate.Text
import           Marvin.Util.Config              as C
import           Marvin.Util.Regex
import           Options.Applicative
import           Prelude                         hiding (dropWhile, splitAt)
import           Util


vcatMaybes :: Vector (Maybe b) -> Vector b
vcatMaybes = V.map fromJust . V.filter isJust


-- | Command line options for marvin.
data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    , verbose    :: Bool
    , debug      :: Bool
    }


-- | Default name for the config file
defaultConfigName :: FilePath
defaultConfigName = "config.cfg"


defaultLoggingLevel :: LogLevel
defaultLoggingLevel = LevelWarn


-- | Retrieve a value from the application config, given the whole config structure. Fails if value not parseable as @a@ or not present.
requireFromAppConfig :: (Configured a, MonadIO m) => Config -> Name -> m a
requireFromAppConfig cfg n = do
    subconf <- subconfig (unwrapScriptId applicationScriptId) cfg
    require subconf n


-- | Retrieve a value from the application config, given the whole config structure.
-- Returns 'Nothing' if value not parseable as @a@ or not present.
lookupFromAppConfig :: (C.Configured a, MonadIO m) => C.Config -> C.Name -> m (Maybe a)
lookupFromAppConfig cfg n = do
    subconf <- C.subconfig (unwrapScriptId applicationScriptId) cfg
    C.lookup subconf n


runWAda :: a -> C.Config -> AdapterM a r -> RunnerM r
runWAda ada cfg ac = runReaderT (runAdapterAction ac) (AdapterMEnv cfg ada)

-- TODO add timeouts for handlers
runHandlers :: forall a. IsAdapter a => Handlers a -> Chan (Event a) -> RunnerM ()
runHandlers handlers eventChan = forever $ readChan eventChan >>= void . async . genericHandler
  where
    genericHandler ev = do
        generics <- mapM async $ vcatMaybes $ fmap ($ ev) customsV
        handler ev
        mapM_ wait generics
    handler (MessageEvent user chan msg ts) = handleMessage user chan msg ts
    handler (CommandEvent user chan msg ts) = handleCommand user chan msg ts
    -- TODO implement other handlers
    handler (ChannelJoinEvent user chan ts) = changeHandlerHelper joinsV joinsInV (User' user, , ts) chan
    handler (ChannelLeaveEvent user chan ts) = changeHandlerHelper leavesV leavesFromV (User' user, , ts) chan
    handler (TopicChangeEvent user chan topic ts) = changeHandlerHelper topicsV topicsInV (User' user, , topic, ts) chan
    handler (FileSharedEvent user chan file ts) = changeHandlerHelper fileSharesV fileSharesInV (User' user, , File' file, ts) chan

    changeHandlerHelper :: Vector (d -> RunnerM ())
                        -> HM.HashMap L.Text (Vector (d -> RunnerM ()))
                        -> (Channel' a -> d)
                        -> Channel a
                        -> RunnerM ()
    changeHandlerHelper wildcards specifics other chan =
        mapConcurrently_ ($ other (Channel' chan)) $ wildcards <> applicables
      where applicables = fromMaybe mempty $ specifics^?ix (chan ^.name)


    handleMessageLike :: Vector (Regex, (User' a, Channel' a, Match, Message, TimeStamp a) -> RunnerM ())
                      -> User a
                      -> Channel a
                      -> Message
                      -> TimeStamp a
                      -> RunnerM ()
    handleMessageLike v user chan msg ts = mapConcurrently_ id $ doIfMatch v
      where
        doIfMatch = vcatMaybes . fmap select
        select (trigger, msgHandler) = (\m -> msgHandler (User' user, Channel' chan, m, msg, ts)) <$> match trigger msg
    handleCommand = handleMessageLike respondsV
    handleMessage = handleMessageLike hearsV

    Handlers respondsV hearsV customsV joinsV leavesV topicsV fileSharesV joinsInV leavesFromV topicsInV fileSharesInV = handlers


initHandlers :: IsAdapter a => [ScriptInit a] -> Config -> a -> RunnerM (Handlers a)
initHandlers inits botConfig ada = do
    logInfoNS logSource "Initializing scripts"
    !handlers <- force . foldMap (^.actions) . catMaybes <$> mapM (\(ScriptInit (sid, s)) -> catch (Just <$> s ada botConfig) (onInitExcept sid)) inits
    return handlers
  where
    logSource = $(isT "#{applicationScriptId}.init")
    onInitExcept :: ScriptId -> SomeException -> RunnerM (Maybe a')
    onInitExcept (ScriptId sid) e = do
        err $(isT "Unhandled exception during initialization of script #{sid}")
        err $(isT "#{e}")
        return Nothing
      where err = logErrorNS logSource


-- | Sets the logging level for the nested logger monad
setLoggingLevelIn :: LogLevel -> RunnerM a -> RunnerM a
setLoggingLevelIn lvl = filterLogger f
    where f _ lvl2 = lvl2 >= lvl


-- | Parse the standard marvin command line args from the environment
parseMarvinCmdArgs :: MonadIO m => m CmdOptions
parseMarvinCmdArgs = liftIO $ execParser marvinCmdArgsParser


-- | The parser for the standard command line arguments
marvinCmdArgsParser :: ParserInfo CmdOptions
marvinCmdArgsParser = info
    (helper <*> optsParser)
    (fullDesc <> header "Instance of marvin, the modular bot.")
  where
    optsParser = CmdOptions
        <$> optional
            ( strOption
            $  long "config-path"
            <> value defaultConfigName
            <> short 'c'
            <> metavar "PATH"
            <> help "root cofiguration file for the bot"
            <> showDefault
            )
        <*> switch
            (  long "verbose"
            <> short 'v'
            <> help "enable verbose logging (overrides config)"
            )
        <*> switch
            (  long "debug"
            <> help "enable debug logging (overrides config and verbose flag)"
            )


getLoggingLevel :: MonadIO m => Config -> CmdOptions -> m LogLevel
getLoggingLevel cfg args = do
    loggingLevelFromCfg <- liftIO $ fmap unwrapLogLevel' <$> C.lookup cfg $(isT "#{applicationScriptId}.logging")

    pure $ if | debug args -> LevelDebug
              | verbose args -> LevelInfo
              | otherwise -> fromMaybe defaultLoggingLevel loggingLevelFromCfg


-- | Runs the marvin bot using whatever method the adapter uses.
runMarvin :: IsAdapter a => [ScriptInit a] -> IO ()
runMarvin s' = runStderrLoggingT $ do
    args <- parseMarvinCmdArgs

    cfgLoc <- maybe
                    (logInfoNS $(isT "#{applicationScriptId}") $(isT "Using default config: #{defaultConfigName}")
                        >> return defaultConfigName)
                    return
                    (configPath args)
    (cfgRaw, _) <- liftIO $ Cfg.autoReload Cfg.autoConfig [Cfg.Required cfgLoc]
    let cfg = Config cfgRaw
    !loggingLevel <- getLoggingLevel cfg args
    setLoggingLevelIn loggingLevel $ runMarvinWithConfig cfg s'


-- | Run marvin on a custom implementation of config and on a custom logger
-- This function does not change the logger in any way, meaning things such as logging level have to be set before.
runMarvinWithConfig :: forall a. IsAdapter a => Config -> [ScriptInit a] -> RunnerM ()
runMarvinWithConfig cfg s' = do
    oldLogFn <- askLoggerIO
    let runAdaLogging = liftIO . flip runLoggingT (loggingAddSourcePrefix adapterPrefix oldLogFn)
    ada <- runAdaLogging initAdapter
    handlers <- initHandlers s' cfg ada
    eventChan <- newChan
    a <- async $ runWAda ada cfg $ runAdapter (writeChan eventChan)
    link a
    runHandlers handlers eventChan

  where
    adapterPrefix = $(isT "adapter.#{adapterId :: AdapterId a}")


