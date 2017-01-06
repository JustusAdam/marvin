{-|
Module      : $Header$
Description : Running marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
module Marvin.Run
    ( runMarvin, ScriptInit, IsAdapter
    , requireFromAppConfig, lookupFromAppConfig, defaultConfigName
    ) where


import           Control.Concurrent.Async.Lifted (async, wait)
import           Control.Exception.Lifted
import           Control.Lens                    hiding (cons)
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Configurator               as C
import qualified Data.Configurator.Types         as C
import           Data.Foldable                   (for_)
import qualified Data.HashMap.Strict             as HM
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           Data.Monoid                     ((<>))
import           Data.Sequences
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Data.Traversable                (for)
import           Data.Vector                     (Vector)
import           Marvin.Adapter                  as A
import           Marvin.Internal
import           Marvin.Internal.Types           hiding (channel)
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex
import           Options.Applicative
import           Prelude                         hiding (dropWhile, splitAt)
import           Util


data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    , verbose    :: Bool
    , debug      :: Bool
    }


defaultBotName :: L.Text
defaultBotName = "marvin"


defaultConfigName :: FilePath
defaultConfigName = "config.cfg"


defaultLoggingLevel :: LogLevel
defaultLoggingLevel = LevelWarn


requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)


declareFields [d|
    data Handlers f = Handlers
        { handlersResponds :: f (Regex, Message -> Match -> RunnerM ())
        , handlersHears :: f (Regex, Message -> Match -> RunnerM ())
        , handlersCustoms :: f (Event -> Maybe (RunnerM ()))
        , handlersJoins :: f ((User, Channel) -> RunnerM ())
        , handlersLeaves :: f ((User, Channel) -> RunnerM ())
        , handlersTopicChange :: f ((Topic, Channel) -> RunnerM ())
        , handlersJoinsIn :: HM.HashMap L.Text (f ((User, Channel) -> RunnerM ()))
        , handlersLeavesFrom :: HM.HashMap L.Text (f ((User, Channel) -> RunnerM ()))
        , handlersTopicChangeIn :: HM.HashMap L.Text (f ((Topic, Channel) -> RunnerM ()))
        }
    |]


mapHandlerFunctor :: (forall a. f a -> f' a) -> Handlers f -> Handlers f'
mapHandlerFunctor f (Handlers respondsV hearsV customsV joinsV leavesV topicsV joinsInV leavesFromV topicsInV) =
    Handlers (f respondsV) (f hearsV) (f customsV) (f joinsV) (f leavesV) (f topicsV)
             (fmap f joinsInV) (fmap f leavesFromV) (fmap f topicsInV)


-- TODO add timeouts for handlers
mkApp :: IsAdapter a => LoggingFn -> [Script a] -> C.Config -> a -> EventHandler a
mkApp log scripts cfg adapter = flip runLoggingT log . genericHandler
  where
    genericHandler ev = do
        generics <- async $ do
            let applicables = catMaybes $ fmap ($ ev) customsV
            asyncs <- for applicables async
            for_ asyncs wait
        handler ev
        wait generics
    handler (MessageEvent msg) = handleMessage msg
    -- TODO implement other handlers
    handler (ChannelJoinEvent user chan) = changeHandlerHelper joinsV joinsInV user chan
    handler (ChannelLeaveEvent user chan) = changeHandlerHelper leavesV leavesFromV user chan
    handler (TopicChangeEvent topic chan) = changeHandlerHelper topicsV topicsInV topic chan

    changeHandlerHelper :: Vector ((b, Channel) -> RunnerM ())
                        -> (HM.HashMap L.Text (Vector ((b, Channel) -> RunnerM ())))
                        -> b
                        -> Channel
                        -> RunnerM ()
    changeHandlerHelper wildcards specifics other chan = do
        cName <- A.getChannelName adapter chan

        let applicables = fromMaybe mempty $ specifics^?ix cName

        wildcards <- for wildcards (async . ($ (other, chan)))

        applicablesRunning <- for applicables (async . ($ (other, chan)))

        mapM_ wait $ wildcards `mappend` applicablesRunning



    handleMessage msg = do
        lDispatches <- doIfMatch hearsV text
        botname <- fromMaybe defaultBotName <$> liftIO (lookupFromAppConfig cfg "name")
        let (trimmed, remainder) = L.splitAt (fromIntegral $ succ $ L.length botname) $ L.stripStart text
        -- TODO At some point this needs to support derivations of the name. Maybe make that configurable?
        rDispatches <- if L.stripEnd (L.toLower trimmed) == L.strip (L.toLower botname)
                            then doIfMatch respondsV remainder
                            else return mempty
        mapM_ wait (lDispatches <> rDispatches)
      where
        text = content msg
        doIfMatch things toMatch  =
            catMaybes <$> for things (\(trigger, action) ->
                case match trigger toMatch of
                        Nothing -> return Nothing
                        Just m  -> Just <$> async (action msg m))

    flattenActions = foldr $ \script -> flip (foldr (addAction script adapter)) (script^.actions)

    Handlers respondsV hearsV customsV joinsV leavesV topicsV joinsInV leavesFromV topicsInV =
        (mapHandlerFunctor fromList :: Handlers [] -> Handlers Vector)
        $ flattenActions (Handlers mempty mempty mempty mempty mempty mempty mempty mempty mempty :: Handlers []) scripts


addAction :: forall a. Script a -> a -> WrappedAction a -> Handlers [] -> Handlers []
addAction script adapter wa =
    case wa of
        WrappedAction (Hear re) ac -> hears %~ cons (re, runMessageAction script adapter re ac)
        WrappedAction (Respond re) ac -> responds %~ cons (re, runMessageAction script adapter re ac)
        WrappedAction Join ac -> joins %~ cons (\d -> botAcWith (Just "Join event" :: Maybe T.Text) d ac)
        WrappedAction Leave ac -> leaves %~ cons (\d -> botAcWith (Just "Leave event" :: Maybe T.Text) d ac)
        WrappedAction (JoinIn chName) ac -> joinsIn . at chName %~ Just . maybe (return reac) (cons reac)
          where reac chan = botAcWith (Just "Join event" :: Maybe T.Text) chan ac
        WrappedAction (LeaveFrom chName) ac -> leavesFrom . at chName %~ Just . maybe (return reac) (cons reac)
          where reac chan = botAcWith (Just "Leave event" :: Maybe T.Text) chan ac
        WrappedAction TopicC ac -> topicChange %~ cons (\d -> botAcWith (Just "Topic event" :: Maybe T.Text) d ac)
        WrappedAction (TopicCIn chanName) ac -> topicChangeIn . at chanName %~ Just . maybe (return reac) (cons reac)
          where reac chan = botAcWith (Just "Topic event" :: Maybe T.Text) chan ac
        WrappedAction (Custom matcher) ac -> customs %~ cons h
          where
            h ev = run <$> matcher ev
            run s = botAcWith (Nothing :: Maybe ()) s ac
  where
    botAcWith :: ShowT t =>  Maybe t -> d -> BotReacting a d () -> RunnerM ()
    botAcWith = runBotAction script adapter


runBotAction :: ShowT t => Script a -> a -> Maybe t -> d -> BotReacting a d () -> RunnerM ()
runBotAction script adapter trigger data_ action = do
    oldLogFn <- askLoggerIO
    catch
        (liftIO $ flip runLoggingT (loggingAddSourcePrefix $(isT "script.#{script^.scriptId}") oldLogFn) $ flip runReaderT actionState $ runReaction action)
        (onScriptExcept (script^.scriptId) trigger)

  where
    actionState = BotActionState (script^.scriptId) (script^.config) adapter data_


runMessageAction :: Script a -> a -> Regex -> BotReacting a MessageReactionData () -> Message -> Match -> RunnerM ()
runMessageAction script adapter re ac msg mtch =
    runBotAction script adapter (Just re) (MessageReactionData msg mtch) ac


onScriptExcept :: ShowT t => ScriptId -> Maybe t -> SomeException -> RunnerM ()
onScriptExcept id trigger e = do
    case trigger of
        Just t ->
            err $(isT "Unhandled exception during execution of script #{id} with trigger #{t}")
        Nothing ->
            err $(isT "Unhandled exception during execution of script #{id}")
    err $(isT "#{e}")
  where
    err = logErrorNS "#{applicationScriptId}.dispatch"


-- | Create a wai compliant application
application :: IsAdapter a => LoggingFn -> [ScriptInit a] -> C.Config -> InitEventHandler a
application log inits config ada = flip runLoggingT log $ do
    $logInfoS "bot" "Initializing scripts"
    s <- catMaybes <$> mapM (\(ScriptInit (sid, s)) -> catch (Just <$> s ada config) (onInitExcept sid)) inits
    return $ mkApp log s config ada
  where
    onInitExcept :: ScriptId -> SomeException -> RunnerM (Maybe a')
    onInitExcept (ScriptId id) e = do
        err $(isT "Unhandled exception during initialization of script ${id}")
        err $(isT "#{e}")
        return Nothing
      where err = logErrorNS $(isT "#{applicationScriptId}.init")


setLoggingLevelIn :: LogLevel -> RunnerM a -> RunnerM a
setLoggingLevelIn lvl = filterLogger f
    where f _ lvl2 = lvl2 >= lvl


-- | Runs the marvin bot using whatever method the adapter uses.
runMarvin :: forall a. IsAdapter a => [ScriptInit a] -> IO ()
runMarvin s' = runStderrLoggingT $ do
    -- prepareLogger
    args <- liftIO $ execParser infoParser

    cfgLoc <- maybe
                    ($logInfoS $(isT "${applicationScriptId}") "Using default config: config.cfg" >> return defaultConfigName)
                    return
                    (configPath args)
    (cfg, cfgTid) <- liftIO $ C.autoReload C.autoConfig [C.Required cfgLoc]
    loggingLevelFromCfg <- liftIO $ C.lookup cfg $(isT "#{applicationScriptId}.logging")

    let loggingLevel
            | debug args = LevelDebug
            | verbose args = LevelInfo
            | otherwise = fromMaybe defaultLoggingLevel loggingLevelFromCfg

    setLoggingLevelIn loggingLevel $ do
        oldLogFn <- askLoggerIO
        liftIO $ flip runLoggingT (loggingAddSourcePrefix adapterPrefix oldLogFn) $ runWithAdapter
            (C.subconfig adapterPrefix cfg)
            $ application oldLogFn s' cfg
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

