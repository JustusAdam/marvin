{-|
Module      : $Header$
Description : Running marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Marvin.Run
    ( runMarvin, ScriptInit, IsAdapter
    , requireFromAppConfig, lookupFromAppConfig, defaultConfigName
    ) where


import           Control.Concurrent.Async.Lifted (async, wait)
import           Control.DeepSeq
import           Control.Exception.Lifted
import           Control.Lens                    hiding (cons)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Configurator               as C
import qualified Data.Configurator.Types         as C
import           Data.Foldable                   (for_)
import qualified Data.HashMap.Strict             as HM
import           Data.Maybe                      (catMaybes, fromJust, fromMaybe, isJust)
import           Data.Monoid                     ((<>))
import qualified Data.Text.Lazy                  as L
import           Data.Traversable                (for)
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import qualified Marvin.Adapter                  as A
import           Marvin.Internal
import           Marvin.Internal.Types           hiding (channel)
import           Marvin.Internal.Values
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex
import           Options.Applicative
import           Prelude                         hiding (dropWhile, splitAt)
import           Util


vcatMaybes = V.map fromJust . V.filter isJust


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
requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


-- | Retrieve a value from the application config, given the whole config structure. Returns 'Nothing' if value not parseable as @a@ or not present.
lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)


runWAda :: a -> C.Config -> AdapterM a r -> RunnerM r
runWAda ada cfg ac = runReaderT (runAdapterAction ac) (cfg, ada)

-- TODO add timeouts for handlers
mkApp :: forall a. IsAdapter a => LoggingFn -> Handlers a -> C.Config -> a -> EventHandler a
mkApp log handlers cfg adapter = flip runLoggingT log . genericHandler
  where
    genericHandler ev = do
        generics <- async $ do
            let applicables = vcatMaybes $ fmap ($ ev) customsV
            asyncs <- for applicables async
            for_ asyncs wait
        handler ev
        wait generics
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
    changeHandlerHelper wildcards specifics other chan = do
        cName <- runWAda adapter cfg $ A.getChannelName chan

        let applicables = fromMaybe mempty $ specifics^?ix cName

        wildcardsRunning <- for wildcards (async . ($ other (Channel' chan)))

        applicablesRunning <- for applicables (async . ($ other (Channel' chan)))

        mapM_ wait wildcardsRunning
        mapM_ wait applicablesRunning


    handleMessageLike :: Vector (Regex, (User' a, Channel' a, Match, Message, TimeStamp a) -> RunnerM ())
                      -> User a
                      -> Channel a
                      -> Message
                      -> TimeStamp a
                      -> RunnerM ()
    handleMessageLike v user chan msg ts = do
        lDispatches <- doIfMatch v
        mapM_ wait lDispatches
      where
        doIfMatch things  =
            vcatMaybes <$> for things (\(trigger, action) ->
                case match trigger msg of
                        Nothing -> return Nothing
                        Just m  -> Just <$> async (action (User' user, Channel' chan, m, msg, ts)))
    handleCommand = handleMessageLike respondsV
    handleMessage = handleMessageLike hearsV

    Handlers respondsV hearsV customsV joinsV leavesV topicsV fileSharesV joinsInV leavesFromV topicsInV fileSharesInV = handlers


application :: IsAdapter a => LoggingFn -> [ScriptInit a] -> C.Config -> a -> RunnerM (EventHandler a)
application log inits config ada = do
    logInfoNS logSource "Initializing scripts"
    !s <- force . foldMap (^.actions) . catMaybes <$> mapM (\(ScriptInit (sid, s)) -> catch (Just <$> s ada config) (onInitExcept sid)) inits
    return $ mkApp log s config ada
  where
    logSource = $(isT "#{applicationScriptId}.init")
    onInitExcept :: ScriptId -> SomeException -> RunnerM (Maybe a')
    onInitExcept (ScriptId id) e = do
        err $(isT "Unhandled exception during initialization of script #{id}")
        err $(isT "#{e}")
        return Nothing
      where err = logErrorNS logSource


setLoggingLevelIn :: LogLevel -> RunnerM a -> RunnerM a
setLoggingLevelIn lvl = filterLogger f
    where f _ lvl2 = lvl2 >= lvl


-- | Runs the marvin bot using whatever method the adapter uses.
runMarvin :: forall a. IsAdapter a => [ScriptInit a] -> IO ()
runMarvin s' = runStderrLoggingT $ do
    -- prepareLogger
    args <- liftIO $ execParser infoParser

    cfgLoc <- maybe
                    (logInfoNS $(isT "#{applicationScriptId}") $(isT "Using default config: #{defaultConfigName}") >> return defaultConfigName)
                    return
                    (configPath args)
    (cfg, _) <- liftIO $ C.autoReload C.autoConfig [C.Required cfgLoc]
    loggingLevelFromCfg <- liftIO $ C.lookup cfg $(isT "#{applicationScriptId}.logging")

    let !loggingLevel
            | debug args = LevelDebug
            | verbose args = LevelInfo
            | otherwise = fromMaybe defaultLoggingLevel loggingLevelFromCfg

    setLoggingLevelIn loggingLevel $ do
        oldLogFn <- askLoggerIO
        let runAdaLogging = liftIO . flip runLoggingT (loggingAddSourcePrefix adapterPrefix oldLogFn)
        ada <- runAdaLogging initAdapter
        handler <- application oldLogFn s' cfg ada
        runWAda ada cfg $ runWithAdapter handler
  where
    adapterPrefix = $(isT "adapter.#{adapterId :: AdapterId a}")
    infoParser = info
        (helper <*> optsParser)
        (fullDesc <> header "Instance of marvin, the modular bot.")
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

