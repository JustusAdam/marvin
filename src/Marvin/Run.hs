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
{-# LANGUAGE NamedFieldPuns         #-}
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
import           Data.Maybe                      (fromMaybe, mapMaybe)
import           Data.Monoid                     ((<>))
import           Data.Sequences
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Data.Text.Lazy                  as L
import           Data.Traversable                (for)
import           Data.Vector                     (Vector)
import           Marvin.Adapter
import           Marvin.Internal
import           Marvin.Internal.Types           hiding (channel)
import           Marvin.Interpolate.Text
import           Marvin.Util.Regex
import           Options.Applicative
import           Prelude                         hiding (dropWhile, splitAt)
import           System.IO                       (stderr)


data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    , verbose    :: Bool
    , debug      :: Bool
    }


defaultBotName :: L.Text
defaultBotName = "marvin"


defaultConfigName :: FilePath
defaultConfigName = "config.cfg"


requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)


declareFields [d|
    data Handlers = Handlers
        { handlersResponds :: [(Regex, Message -> Match -> RunnerM ())]
        , handlersHears :: [(Regex, Message -> Match -> RunnerM ())]
        , handlersCustoms :: [Event -> Maybe (RunnerM ())]
        }
    |]


-- TODO add timeouts for handlers
mkApp :: [Script a] -> C.Config -> a -> EventHandler a
mkApp scripts cfg adapter = runStderrLoggingT . genericHandler
  where
    genericHandler ev = do
        generics <- async $ do
            let applicables = mapMaybe ($ ev) allCustoms
            asyncs <- for applicables async
            for_ asyncs wait
        handler ev
        wait generics
    handler (MessageEvent msg) = handleMessage msg

    handleMessage msg = do
        lDispatches <- doIfMatch allListens text
        botname <- fromMaybe defaultBotName <$> liftIO (lookupFromAppConfig cfg "name")
        let (trimmed, remainder) = L.splitAt (fromIntegral $ L.length botname) $ L.stripStart text
        -- TODO At some point this needs to support derivations of the name. Maybe make that configurable?
        rDispatches <- if L.toLower trimmed == L.toLower botname
                            then doIfMatch allReactions remainder
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

    allActions = flattenActions (Handlers mempty mempty mempty) scripts

    allReactions :: Vector (Regex, Message -> Match -> RunnerM ())
    allReactions = fromList $! allActions^.responds
    allListens :: Vector (Regex, Message -> Match -> RunnerM ())
    allListens = fromList $! allActions^.hears
    allCustoms :: [Event -> Maybe (RunnerM ())]
    allCustoms = allActions^.customs


addAction :: Script a -> a -> WrappedAction a -> Handlers -> Handlers
addAction script adapter wa =
    case wa of
        (WrappedAction (Hear re) ac) -> hears %~ cons (re, runMessageAction script adapter re ac)
        (WrappedAction (Respond re) ac) -> responds %~ cons (re, runMessageAction script adapter re ac)
        (WrappedAction (Custom matcher) ac) -> customs %~ cons h
          where
            h ev = run <$> matcher ev
            run s = runBotAction script adapter (Nothing :: Maybe ()) s ac


runBotAction :: ShowT t => Script a -> a -> Maybe t -> d -> BotReacting a d () -> RunnerM ()
runBotAction script adapter trigger data_ action =
    catch
        (runReaderT (runReaction action) actionState)
        (onScriptExcept (script^.scriptId) trigger)

  where
    actionState = BotActionState (script^.scriptId) (script^.config) adapter data_


runMessageAction :: Script a -> a -> Regex -> BotReacting a MessageReactionData () -> Message -> Match -> RunnerM ()
runMessageAction script adapter re ac msg mtch =
    runBotAction script adapter (Just re) (MessageReactionData msg mtch) ac


onScriptExcept :: ShowT t => ScriptId -> Maybe t -> SomeException -> RunnerM ()
onScriptExcept (ScriptId id) trigger e = do
    case trigger of
        Just t ->
            err $(isT "Unhandled exception during execution of script %{id} with trigger %{t}")
        Nothing ->
            err $(isT "Unhandled exception during execution of script %{id}")
    err $(isT "%{e}")
  where
    err = logErrorNS "bot.dispatch"


-- | Create a wai compliant application
application :: [ScriptInit a] -> C.Config -> InitEventHandler a
application inits config ada = runStderrLoggingT $ do
    $logInfoS "bot" "Initializing scripts"
    s <- catMaybes <$> mapM (\(ScriptInit (sid, s)) -> catch (Just <$> s ada config) (onInitExcept sid)) inits
    return $ mkApp s config ada
  where
    onInitExcept :: ScriptId -> SomeException -> RunnerM (Maybe a')
    onInitExcept (ScriptId id) e = do
        err $(isT "Unhandled exception during initialization of script ${id}")
        err $(isT "%{e}")
        return Nothing
      where err = logErrorNS $(isT "%{applicationScriptId}.init")


-- prepareLogger :: IO ()
-- prepareLogger =
--     L.updateGlobalLogger L.rootLoggerName (L.setHandlers [handler])
--   where
--     handler = L.GenericHandler { L.priority = L.DEBUG
--                                , L.formatter = L.simpleLogFormatter "$time [$prio:$loggername] $msg"
--                                , L.privData = ()
--                                , L.writeFunc = const putStrLn
--                                , L.closeFunc = const $ return ()
--                                }


-- | Runs the marvin bot using whatever method the adapter uses.
runMarvin :: forall a. IsAdapter a => [ScriptInit a] -> RunnerM ()
runMarvin s' = do
    -- prepareLogger
    args <- liftIO $ execParser infoParser
    -- when (verbose args) $ L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)
    -- when (debug args) $ L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
    cfgLoc <- maybe
                ($logInfoS $(isT "${applicationScriptId}") "Using default config: config.cfg" >> return defaultConfigName)
                return
                (configPath args)
    (cfg, cfgTid) <- liftIO $ C.autoReload C.autoConfig [C.Required cfgLoc]
    -- unless (verbose args || debug args) $ C.lookup cfg "bot.logging" >>= maybe (return ()) (L.updateGlobalLogger L.rootLoggerName . L.setLevel)

    runWithAdapter
        (C.subconfig $(isT "adapter.%{adapterId :: AdapterId a}") cfg)
        $ application s' cfg
  where
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

