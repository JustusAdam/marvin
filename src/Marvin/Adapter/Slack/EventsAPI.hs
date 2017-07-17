{-|
Module      : $Header$
Description : Adapter for communicating with Slack via the webhook based Events API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

See http://marvin.readthedocs.io/en/latest/adapters.html#events-api for documentation about this adapter.
-}
module Marvin.Adapter.Slack.EventsAPI
    ( SlackAdapter, EventsAPI
    , SlackUserId, SlackChannelId
    , MkSlack
    , SlackRemoteFile(..), SlackLocalFile(..)
    , HasTitle(..), HasPublicPermalink(..), HasEditable(..), HasPublic(..), HasUser(..), HasPrivateUrl(..), HasComment(..)
    ) where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as L
import qualified Data.Text.Lazy.Encoding              as L
import           Lens.Micro.Platform
import           Marvin.Adapter
import           Marvin.Adapter.Slack.Internal.Common
import           Marvin.Adapter.Slack.Internal.Types
import           Marvin.Interpolate.All
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Network.Wreq


eventAPIeventParser :: Value -> Parser (T.Text, Either L.Text (InternalType EventsAPI))
eventAPIeventParser = withObject "expected object" $ \o -> do
    token <- o .: "token"
    type_ <- o .: "type"

    (token,) <$> case (type_ :: T.Text) of
        "url_verification" -> Left <$> o .: "challenge"
        "event_callback"   -> Right <$> (o .: "event" >>= eventParser)
        _                  -> fail "unknown wrapper event type"


runEventReceiver :: Chan (InternalType EventsAPI) -> AdapterM (SlackAdapter EventsAPI) ()
runEventReceiver evChan = do
    useTLS <- fromMaybe True <$> lookupFromAdapterConfig "use-tls"
    server <- if useTLS
        then do
            certfile <- requireFromAdapterConfig "certfile"
            keyfile <- requireFromAdapterConfig "keyfile"
            return $ runTLS $ tlsSettings certfile keyfile
        else return runSettings
    port <- fromMaybe 7000 <$> lookupFromAdapterConfig "port"
    expectedToken <- requireFromAdapterConfig "token"

    let warpSet = setPort port defaultSettings

    logFn <- askLoggerIO

    liftIO $ server warpSet $ \req resp -> flip runLoggingT logFn $
        let
            respond status rheaders body = liftIO $ resp $ responseLBS status rheaders body
        in  if requestMethod req == methodPost
                then do
                    bod <- liftIO $ lazyRequestBody req
                    case eitherDecode bod >>= parseEither eventAPIeventParser of
                        Left err -> do
                            logErrorN $(isT "Unreadable JSON event: '#{err}'")
                            respond notAcceptable406 [] ""
                        Right (token,_) | token /= expectedToken -> do
                            logErrorN $(isT "Recieved incorrect token: '#{token}'")
                            respond unauthorized401 [] ""
                        Right (_, Left challenge) -> do
                            logInfoN $(isT "Recieved challenge event: '#{challenge}'")
                            respond ok200 [] (L.encodeUtf8 challenge)
                        Right (_, Right ev) -> do
                            writeChan evChan ev
                            respond ok200 [] ""
                else respond methodNotAllowed405 [] ""


sendMessageLoop :: AdapterM (SlackAdapter EventsAPI) ()
sendMessageLoop = do
    outChan <- view (adapter.outChannel)
    forever $ do
        (SlackChannelId chan, msg) <- readChan outChan
        either (\err -> logErrorN $(isT "Sending message failed: #{err}")) (const $ return ()) =<<
            execAPIMethod
                (const $ return ())
                "chat.postMessage"
                [ "channel" := chan
                , "text" := msg
                ]


-- | Recieve events as a server via HTTP webhook
data EventsAPI


instance MkSlack EventsAPI where
    mkAdapterId = "slack-events"
    initIOConnections inChan = do
        a <- async $ runEventReceiver inChan
        link a
        sendMessageLoop
