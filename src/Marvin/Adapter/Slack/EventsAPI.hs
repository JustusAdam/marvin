{-|
Module      : $Header$
Description : Adapter for communicating with Slack via the webhook based Events API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Adapter.Slack.EventsAPI
    ( SlackAdapter, EventsAPI
    , SlackUserId, SlackChannelId
    , MkSlack
    ) where


import           Control.Concurrent.Chan.Lifted
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import qualified Data.Text.Lazy                 as L
import qualified Data.Text.Lazy.Encoding        as L
import           Marvin.Adapter
import           Marvin.Adapter.Slack.Common
import           Marvin.Adapter.Slack.Types
import           Marvin.Interpolate.Text
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS


eventAPIeventParser :: Value -> Parser (T.Text, Either L.Text (Either InternalType (Event (SlackAdapter a))))
eventAPIeventParser = withObject "expected object" $ \o -> do
    token <- o .: "token"
    type_ <- o .: "type"

    (token,) <$> case (type_ :: T.Text) of
        "url_verification" -> Left <$> o .: "challenge"
        "event_callback" -> Right <$> (o .: "event" >>= eventParser)
        _ -> fail "unknown wrapper event type"


runEventReceiver :: Chan (Either InternalType (Event (SlackAdapter EventsAPI))) -> AdapterM (SlackAdapter EventsAPI) ()
runEventReceiver evChan = do
    certfile <- requireFromAdapterConfig "certfile"
    keyfile <- requireFromAdapterConfig "keyfile"
    port <- fromMaybe 7000 <$> lookupFromAdapterConfig "port"
    expectedToken <- requireFromAdapterConfig "token"

    let tlsSet = tlsSettings certfile keyfile
        warpSet = setPort port defaultSettings

    logFn <- askLoggerIO

    liftIO $ runTLS tlsSet warpSet $ \req resp -> flip runLoggingT logFn $ do
        let meth = requestMethod req
        if meth == methodPost
            then do
                bod <- liftIO $ lazyRequestBody req
                case eitherDecode bod >>= parseEither eventAPIeventParser of
                    Left err -> do
                        logErrorN $(isT "Unreadable JSON event: #{err}")
                        liftIO $ resp $ responseLBS notAcceptable406 [] ""
                    Right (token,_) | token /= expectedToken -> do
                        logErrorN $(isT "Recieved incorrect token: #{token}")
                        liftIO $ resp $ responseLBS unauthorized401 [] ""
                    Right (_, Left challenge) -> do
                        logInfoN $(isT "Recieved challenge event: '#{challenge}'")
                        liftIO $ resp $ responseLBS ok200 [] (L.encodeUtf8 challenge)
                    Right (_, Right ev) -> do
                        writeChan evChan ev
                        liftIO $ resp $ responseLBS ok200 [] ""
            else liftIO $ resp $ responseLBS methodNotAllowed405 [] ""


-- | Recieve events as a server via HTTP webhook (not implemented yet)
data EventsAPI


instance MkSlack EventsAPI where
    mkAdapterId = "slack-events"
    initIOConnections = error "not implemented"
