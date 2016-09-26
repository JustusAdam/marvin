{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Framework.SlackBot.Server where


import ClassyPrelude
import Network.Wai as Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Vector (Vector)
import Framework.SlackBot.Logging
import Framework.SlackBot.Types
import Framework.SlackBot.Internal
import Options.Generic
import Control.Lens
import Data.Time


data CmdOptions = CmdOptions
    { configPath :: Maybe FilePath
    } deriving (Generic)


instance ParseRecord CmdOptions


data EventType 
    = MessageEvent
        { user :: User
        , channel :: Text
        , text :: Text
        , ts :: LocalTime
        }


data EventCallback 
    = UrlVerification
        { token :: Text 
        , challenge :: Text
        } 
    | EventCallback
        { token :: Text
        , teamId :: Text
        , apiAppId :: Text
        , event :: EventType
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


mkApp :: [Script] -> C.Config -> Request -> IO Wai.Response
mkApp scripts config = handler
  where
    handler request = do 
        bod <- lazyRequestBody request
        case eitherDecode' bod of
            Left err -> do
                logMsg err
                return $ responseLBS ok200 [] "Recieved malformed JSON, check your server log for further information"
            Right v ->
                case v of
                    UrlVerification{challenge} -> return $ responseLBS ok200 [] (fromStrict $ encodeUtf8 challenge)
                    EventCallback{} -> return $ responseLBS ok200 [] ""


application :: [Script] -> C.Config -> Application
application s config = \request respond -> prepared request >>= respond
  where
    prepared = mkApp s config


runServer :: [ScriptInit] -> IO ()
runServer s' = do
    args <- getRecord "bot server"
    let cfgLoc = fromMaybe "config.cfg" $ configPath args
    (cfg, cfgTid) <- C.autoReload C.autoConfig [C.Required cfgLoc]
    s <- mapM (\(ScriptInit s) -> s cfg) s'
    let app = application s cfg
    port <- C.lookup cfg "application.port"
    run (fromMaybe 8080 port) app
