{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Marvin.Server
    ( application
    , runServer
    , runHTTPSServer
    ) where


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


data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    , verbose    :: Bool
    , debug      :: Bool
    } deriving (Generic)


instance ParseRecord CmdOptions


data EventType
    = MessageEvent
        { user    :: User
        , channel :: Room
        , text    :: Text
        , ts      :: LocalTime
        }


data EventCallback
    = UrlVerification
        { token     :: Text
        , challenge :: Text
        }
    | EventCallback
        { token       :: Text
        , teamId      :: Text
        , apiAppId    :: Text
        , event       :: EventType
        , authedUsers :: Vector User
        }


deriveJSON
    defaultOptions { fieldLabelModifier = camelTo2 '_'
                   , constructorTagModifier = camelTo2 '_'
                   , omitNothingFields = True
                   , sumEncoding = TaggedObject "type" "payload"
                   }
    ''EventType

deriveJSON
    defaultOptions { fieldLabelModifier = camelTo2 '_'
                   , constructorTagModifier = camelTo2 '_'
                   , omitNothingFields = True
                   , sumEncoding = TaggedObject "type" "payload"
                   }
    ''EventCallback


declareFields [d|
    data Handlers = Handlers
        { handlersResponds :: Seq (Regex, Message -> Match -> IO ())
        , handlersHears :: Seq (Regex, Message -> Match -> IO ())
        }
    |]



defaultBotName :: Text
defaultBotName = "marvin"


requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)


mkApp :: [Script] -> C.Config -> Request -> IO Wai.Response
mkApp scripts cfg = handler
  where
    handler request = do
        bod <- lazyRequestBody request
        case eitherDecode' bod of
            Left err -> do
                noticeAccept "Recieved malformed JSON"
                noticeAccept err
                return $ responseLBS ok200 [] "Recieved malformed JSON, check your server log for further information"
            Right v -> do
                tkn <- requireFromAppConfig cfg "token"
                if token v == tkn
                    then handleApiRequest v
                    else do
                        noticeAccept "Unathorized request recieved (token invalid)"
                        return $ responseLBS unauthorized401 [] ""

    noticeAccept = L.noticeM  "server.accept"

    handleApiRequest UrlVerification{challenge} =
        return $ responseLBS ok200 [("Content-Type", "application/x-www-form-urlencoded")] (fromStrict $ encodeUtf8 challenge)
    handleApiRequest EventCallback{event} = do
        async $ handleMessage event
        return $ responseLBS ok200 [] ""

    handleMessage MessageEvent{user, channel, text, ts} = do
        lDispatches <- doIfMatch allListens text
        botname <- fromMaybe defaultBotName <$> lookupFromAppConfig cfg "name"
        let (trimmed, remainder) = splitAt (length botname) $ dropWhile isSpace text
        rDispatches <- if toLower trimmed == toLower botname
                            then doIfMatch allReactions remainder
                            else return mempty
        mapM_ wait (lDispatches ++ rDispatches)
      where
        doIfMatch things toMatch  =
            catMaybes <$> for things (\(trigger, action) ->
                case match trigger toMatch of
                        Nothing -> return Nothing
                        Just m -> Just <$> async (action (Message user channel text ts) m))

    onScriptExcept :: ScriptId -> Regex -> SomeException -> IO ()
    onScriptExcept (ScriptId id) r e = do
        let logger =  "bot.dispatch"
        L.errorM logger $ "Unhandled exception during execution of script " ++ show id ++ " with trigger " ++ show r
        L.errorM logger (show e)

    flattenActions =
        for_ scripts $ \script ->
            for_ (script^.actions) $ addAction script

    addAction :: MonadState Handlers m => Script -> WrappedAction -> m ()
    addAction script (WrappedAction (Hear re) ac) = hears %= cons (re, runAc)
      where
        runAc message match = catch
                    (evalStateT (runReaction ac) (BotActionState (script^.scriptId) (script^.config) (MessageReactionData message match)))
                    (onScriptExcept (script^.scriptId) re)
    addAction script (WrappedAction (Respond re) ac) = responds %= cons (re, runAc)
      where
        runAc message match = catch
                    (evalStateT (runReaction ac) (BotActionState (script^.scriptId) (script^.config) (MessageReactionData message match)))
                    (onScriptExcept (script^.scriptId) re)

    allActions = execState flattenActions (Handlers mempty mempty)

    allReactions = allActions^.responds
    allListens = allActions^.hears


-- | Create a wai compliant application
application :: [Script] -> C.Config -> Application
application s config = \request respond -> prepared request >>= respond
  where
    prepared = mkApp s config


prepareServer :: [ScriptInit] -> IO (Int, Application, C.Config)
prepareServer s' = do
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
    let app = application s cfg
    port <- fromMaybe 8000 <$> lookupFromAppConfig cfg "port"
    L.infoM "bot" $ "Starting server on port " ++ show port
    return (port, app, cfg)
  where
    onInitExcept :: ScriptId -> SomeException -> IO (Maybe a)
    onInitExcept (ScriptId id) e = do
        let logger = "bot.init"
        L.errorM logger $ "Unhandled exception during initialization of script " ++ show id
        L.errorM logger (show e)
        return Nothing


-- | Parses command line arguments and runs a server with the arguments provided there.
runServer :: [ScriptInit] -> IO ()
runServer s = do
    (port, app, _) <- prepareServer s
    run port app


-- | Parses command line arguments and runs a server protected with TLS with the arguments provided there.
runHTTPSServer :: [ScriptInit] -> IO ()
runHTTPSServer s = do
    (port, app, cfg) <- prepareServer s
    certfile <- requireFromAppConfig cfg "certfile"
    keyfile <- requireFromAppConfig cfg "keyfile"
    let tlsSet = tlsSettings certfile keyfile
        warpSet = setPort port defaultSettings
    runTLS tlsSet warpSet app
