{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
module Marvin.Server
    ( application
    , runServer
    ) where


import           ClassyPrelude
import           Control.Concurrent.Async (wait)
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Char                (isSpace)
import qualified Data.Configurator        as C
import qualified Data.Configurator.Types  as C
import           Data.Time
import           Data.Vector              (Vector)
import           Marvin.Internal
import           Marvin.Logging
import           Marvin.Types             hiding (channel)
import           Network.HTTP.Types
import           Network.Wai              as Wai
import           Network.Wai.Handler.Warp
import           Options.Generic


data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
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


defaultBotName :: Text
defaultBotName = "marvin"


requireFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO a
requireFromAppConfig cfg = C.require (C.subconfig (unwrapScriptId applicationScriptId) cfg)


lookupFromAppConfig :: C.Configured a => C.Config -> C.Name -> IO (Maybe a)
lookupFromAppConfig cfg = C.lookup (C.subconfig (unwrapScriptId applicationScriptId) cfg)


mkApp :: [Script] -> C.Config -> Request -> IO Wai.Response
mkApp scripts config = handler
  where
    handler request = do
        bod <- lazyRequestBody request
        case eitherDecode' bod of
            Left err -> do
                logMsg err
                return $ responseLBS ok200 [] "Recieved malformed JSON, check your server log for further information"
            Right v -> do
                tkn <- requireFromAppConfig config "token"
                if (token v == tkn)
                    then handleApiRequest v
                    else return $ responseLBS unauthorized401 [] ""

    handleApiRequest UrlVerification{challenge} =
        return $ responseLBS ok200 [("Content-Type", "application/x-www-form-urlencoded")] (fromStrict $ encodeUtf8 challenge)
    handleApiRequest EventCallback{event} = do
        async $ handleMessage event
        return $ responseLBS ok200 [] ""

    handleMessage MessageEvent{user, channel, text, ts} = do
        lDispatches <- doIfMatch allListens text
        botname <- fromMaybe defaultBotName <$> lookupFromAppConfig config "name"
        let (trimmed, remainder) = splitAt (length botname) $ dropWhile isSpace text
        rDispatches <- if toLower trimmed == toLower botname
                            then doIfMatch allReactions remainder
                            else return []
        void $ mapM wait (lDispatches ++ rDispatches)
      where
        doIfMatch things toMatch  =
            catMaybes <$> for things (\(trigger, action) -> do
                case match trigger toMatch of
                        Nothing -> return Nothing
                        Just m -> Just <$> async (action (Message user channel text ts) m))

    allReactions = prepareActions scriptReactions
    allListens = prepareActions scriptListens
    prepareActions getter =
        [ (trigger, \message match -> evalStateT (runReaction action) (BotAnswerState message (scriptScriptId script) match (scriptConfig script))
          )
        | script <- scripts
        , (trigger, action) <- getter script
        ]


-- | Create a wai compliant application
application :: [Script] -> C.Config -> Application
application s config = \request respond -> prepared request >>= respond
  where
    prepared = mkApp s config


-- | Parses command line arguments and runs a server with the arguments provided there.
runServer :: [ScriptInit] -> IO ()
runServer s' = do
    args <- getRecord "bot server"
    let cfgLoc = fromMaybe "config.cfg" $ configPath args
    (cfg, cfgTid) <- C.autoReload C.autoConfig [C.Required cfgLoc]
    s <- mapM (\(ScriptInit s) -> s cfg) s'
    let app = application s cfg
    port <- lookupFromAppConfig cfg "port"
    run (fromMaybe 8080 port) app
