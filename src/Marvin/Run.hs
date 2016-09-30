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
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Marvin.Run where


import           ClassyPrelude
import           Control.Concurrent.Async    (wait)
import           Control.Lens                hiding (cons)
import           Control.Monad.State         hiding (mapM_)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Char                   (isSpace)
import qualified Data.Configurator           as C
import qualified Data.Configurator.Types     as C
import           Data.Time
import           Data.Vector                 (Vector)
import           Marvin.Internal             hiding (match)
import           Marvin.Internal.Types       hiding (channel)
import           Marvin.Logging
import           Marvin.Regex
import           Network.HTTP.Types
import           Network.Wai                 as Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Options.Generic
import qualified System.Log.Logger           as L
import qualified System.Log.Formatter as L
import qualified System.Log.Handler.Simple as L
import qualified System.Log.Handler as L hiding (setLevel)
import qualified Prelude as P
import Marvin.Adapter



data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    , verbose    :: Bool
    , debug      :: Bool
    } deriving (Generic)


instance ParseRecord CmdOptions


defaultBotName :: Text
defaultBotName = "marvin"


requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)

-- mkApp scripts cfg = handler
--   where
--     handler request = do
--         bod <- lazyRequestBody request
--         case eitherDecode' bod of
--             Left err -> do
--                 noticeAccept "Recieved malformed JSON"
--                 noticeAccept err
--                 return $ responseLBS ok200 [] "Recieved malformed JSON, check your server log for further information"
--             Right v -> do
--                 tkn <- requireFromAppConfig cfg "token"
--                 if token v == tkn
--                     then handleApiRequest v
--                     else do
--                         noticeAccept "Unathorized request recieved (token invalid)"
--                         return $ responseLBS unauthorized401 [] ""

--     noticeAccept = L.noticeM  "server.accept"

--     handleApiRequest UrlVerification{challenge} =
--         return $ responseLBS ok200 [("Content-Type", "application/x-www-form-urlencoded")] (fromStrict $ encodeUtf8 challenge)
--     handleApiRequest EventCallback{event} = do
--         async $ handleMessage event
--         return $ responseLBS ok200 [] ""


declareFields [d|
    data Handlers = Handlers
        { handlersResponds :: [(Regex, Message -> Match -> IO ())]
        , handlersHears :: [(Regex, Message -> Match -> IO ())]
        }
    |]


mkApp :: [Script] -> C.Config -> OutputProvider -> EventHandler
mkApp scripts cfg op = handler
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
                        Just m -> Just <$> async (action msg m))

    flattenActions = foldr $ \script -> flip (foldr (addAction script op)) (script^.actions)

    allActions = flattenActions (Handlers mempty mempty) scripts

    allReactions :: Vector (Regex, Message -> Match -> IO ())
    allReactions = fromList $! allActions^.responds
    allListens :: Vector (Regex, Message -> Match -> IO ())
    allListens = fromList $! allActions^.hears


addAction :: Script -> OutputProvider -> WrappedAction -> Handlers -> Handlers
addAction script provider wa =
    case wa of
        (WrappedAction (Hear re) ac) -> hears %~ cons (re, runMessageAction script provider re ac)
        (WrappedAction (Respond re) ac) -> responds %~ cons (re, runMessageAction script provider re ac)


runMessageAction :: Script -> OutputProvider -> Regex -> BotReacting MessageReactionData () -> Message -> Match -> IO ()
runMessageAction script provider re ac msg mtch = 
    catch
        (evalStateT (runReaction ac) (BotActionState (script^.scriptId) (script^.config) provider (MessageReactionData msg mtch)))
        (onScriptExcept (script^.scriptId) re) 


onScriptExcept :: ScriptId -> Regex -> SomeException -> IO ()
onScriptExcept (ScriptId id) r e = do
    err $ "Unhandled exception during execution of script " ++ show id ++ " with trigger " ++ show r
    err $ show e
  where
    err = L.errorM "bot.dispatch"


-- | Create a wai compliant application
application :: [Script] -> C.Config -> OutputProvider -> EventHandler
application s config o = prepared
  where
    prepared = mkApp s config o


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



runMarvin :: [ScriptInit] -> BuildAdapter -> IO ()
runMarvin s' builder = do
    prepareLogger
    args <- getRecord "bot server"
    when (verbose args) $ L.updateGlobalLogger L.rootLoggerName (L.setLevel L.INFO)
    when (debug args) $ L.updateGlobalLogger L.rootLoggerName (L.setLevel L.DEBUG)
    cfgLoc <- maybe
                (L.noticeM "bot" "Using default config: config.cfg" >> return "config.cfg")
                return
                (configPath args)
    (cfg, cfgTid) <- C.autoReload C.autoConfig [C.Required cfgLoc]
    unless (verbose args || debug args) $ C.lookup cfg "bot.logging" >>= maybe (return ()) (L.updateGlobalLogger L.rootLoggerName . L.setLevel)
    L.infoM "bot" "Initializing scripts"
    s <- catMaybes <$> mapM (\(ScriptInit (sid, s)) -> catch (Just <$> s cfg) (onInitExcept sid)) s'
    ada <- builder^.buildFunction $ C.subconfig ("adapter." ++ unwrapAdapterId (builder^.adapterId)) cfg
    ada^.runner $ application s cfg (ada^.outputProvider)
  where
    onInitExcept :: ScriptId -> SomeException -> IO (Maybe a)
    onInitExcept (ScriptId id) e = do
        err $ "Unhandled exception during initialization of script " ++ show id
        err $ show e
        return Nothing
      where err = L.errorM "bot.init"


-- -- | Parses command line arguments and runs a server with the arguments provided there.
-- runServer :: [ScriptInit] -> IO ()
-- runServer s = do
--     (port, app, cfg) <- prepareServer s
--     isHttps <- lookupFromAppConfig cfg "tls"
--     let runner = case isHttps of
--                     Just True -> runHTTPSServer
--                     _ -> runHTTPServer
--     runner port app cfg


-- -- | Starts a HTTP server on the provided port with the application
-- runHTTPServer :: Int -> Application -> C.Config -> IO ()
-- runHTTPServer port app cfg = do
--     L.noticeM "server.start" $ "Starting HTTP server on port " ++ show port 
--     run port app


-- -- | Starts a TLS protected HTTP server on the provided port with the application
-- runHTTPSServer :: Int -> Application -> C.Config -> IO ()
-- runHTTPSServer port app cfg = do
--     certfile <- requireFromAppConfig cfg "certfile"
--     keyfile <- requireFromAppConfig cfg "keyfile"
--     let tlsSet = tlsSettings certfile keyfile
--         warpSet = setPort port defaultSettings
--     L.noticeM "server.start" $ "Starting TLS protected HTTP server on port " ++ show port
--     runTLS tlsSet warpSet app
