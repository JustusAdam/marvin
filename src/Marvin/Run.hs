{-|
Module      : $Header$
Description : Running marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE DeriveGeneric          #-}
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


import           ClassyPrelude
import           Control.Concurrent.Async  (wait)
import           Control.Lens              hiding (cons)
import           Control.Monad.State       hiding (mapM_)
import           Data.Char                 (isSpace)
import qualified Data.Configurator         as C
import qualified Data.Configurator.Types   as C
import           Data.Vector               (Vector)
import           Marvin.Adapter
import           Marvin.Internal           hiding (match)
import           Marvin.Internal.Types     hiding (channel)
import           Marvin.Util.Regex
import           Options.Generic
import qualified Prelude                   as P
import qualified System.Log.Formatter      as L
import qualified System.Log.Handler.Simple as L
import qualified System.Log.Logger         as L



data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    , verbose    :: Bool
    , debug      :: Bool
    } deriving (Generic)


instance ParseRecord CmdOptions


defaultBotName :: Text
defaultBotName = "marvin"


defaultConfigName :: FilePath
defaultConfigName = "config.cfg"


requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)


declareFields [d|
    data Handlers = Handlers
        { handlersResponds :: [(Regex, Message -> Match -> IO ())]
        , handlersHears :: [(Regex, Message -> Match -> IO ())]
        }
    |]


mkApp :: [Script a] -> C.Config -> a -> EventHandler a
mkApp scripts cfg adapter = handler
  where
    handler (MessageEvent msg) = handleMessage msg

    handleMessage msg = do
        lDispatches <- doIfMatch allListens text
        botname <- fromMaybe defaultBotName <$> lookupFromAppConfig cfg "name"
        let (trimmed, remainder) = splitAt (length botname) $ dropWhile isSpace text
        rDispatches <- if toLower trimmed == toLower botname
                            then doIfMatch allReactions remainder
                            else return mempty
        mapM_ wait (lDispatches ++ rDispatches)
      where
        text = content msg
        doIfMatch things toMatch  =
            catMaybes <$> for things (\(trigger, action) ->
                case match trigger toMatch of
                        Nothing -> return Nothing
                        Just m  -> Just <$> async (action msg m))

    flattenActions = foldr $ \script -> flip (foldr (addAction script adapter)) (script^.actions)

    allActions = flattenActions (Handlers mempty mempty) scripts

    allReactions :: Vector (Regex, Message -> Match -> IO ())
    allReactions = fromList $! allActions^.responds
    allListens :: Vector (Regex, Message -> Match -> IO ())
    allListens = fromList $! allActions^.hears


addAction :: Script a -> a -> WrappedAction a -> Handlers -> Handlers
addAction script adapter wa =
    case wa of
        (WrappedAction (Hear re) ac) -> hears %~ cons (re, runMessageAction script adapter re ac)
        (WrappedAction (Respond re) ac) -> responds %~ cons (re, runMessageAction script adapter re ac)


runMessageAction :: Script a -> a -> Regex -> BotReacting a MessageReactionData () -> Message -> Match -> IO ()
runMessageAction script adapter re ac msg mtch =
    catch
        (evalStateT (runReaction ac) (BotActionState (script^.scriptId) (script^.config) adapter (MessageReactionData msg mtch)))
        (onScriptExcept (script^.scriptId) re)


onScriptExcept :: ScriptId -> Regex -> SomeException -> IO ()
onScriptExcept (ScriptId id) r e = do
    err $ "Unhandled exception during execution of script " ++ show id ++ " with trigger " ++ show r
    err $ show e
  where
    err = L.errorM "bot.dispatch"


-- | Create a wai compliant application
application :: [ScriptInit a] -> C.Config -> InitEventHandler a
application inits config ada = do
    L.infoM "bot" "Initializing scripts"
    s <- catMaybes <$> mapM (\(ScriptInit (sid, s)) -> catch (Just <$> s ada config) (onInitExcept sid)) inits
    return $ mkApp s config ada
  where
    onInitExcept :: ScriptId -> SomeException -> IO (Maybe a')
    onInitExcept (ScriptId id) e = do
        err $ "Unhandled exception during initialization of script " ++ show id
        err $ show e
        return Nothing
      where err = L.errorM "bot.init"


prepareLogger :: IO ()
prepareLogger =
    L.updateGlobalLogger L.rootLoggerName (L.setHandlers [handler])
  where
    handler = L.GenericHandler { L.priority = L.DEBUG
                               , L.formatter = L.simpleLogFormatter "$time [$prio:$loggername] $msg"
                               , L.privData = ()
                               , L.writeFunc = const P.putStrLn
                               , L.closeFunc = const $ return ()
                               }



runMarvin :: forall a. IsAdapter a => [ScriptInit a] -> IO ()
runMarvin s' = do
    prepareLogger
    args <- getRecord "bot server"
    when (verbose args) $ L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)
    when (debug args) $ L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
    cfgLoc <- maybe
                (L.noticeM "bot" "Using default config: config.cfg" >> return defaultConfigName)
                return
                (configPath args)
    (cfg, cfgTid) <- C.autoReload C.autoConfig [C.Required cfgLoc]
    unless (verbose args || debug args) $ C.lookup cfg "bot.logging" >>= maybe (return ()) (L.updateGlobalLogger L.rootLoggerName . L.setLevel)

    runWithAdapter
        (C.subconfig ("adapter." ++ unwrapAdapterId (adapterId :: AdapterId a)) cfg)
        $ application s' cfg

