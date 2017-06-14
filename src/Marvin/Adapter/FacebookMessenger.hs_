{-# LANGUAGE MultiWayIf #-}
module Marvin.Adapter.FacebookMessenger where


data FMAdapter

newtype UserId = UserId { unwrapUserID :: Text }

data FMUser

data ChannelId

data FMChannel


data FMEvent
    = ChallengeEvent


recieveEvents :: Chan  FMRequest -> AdapterM ()
recieveEvents chan = do
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
        in  if  | requestMethod req == methodPost -> do
                    bod <- liftIO $ lazyRequestBody req
                    case eitherDecode bod >>= parseEither fmEventParser of
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
                | requestMethod req == methodGet ->
                | otherwise -> respond methodNotAllowed405 [] ""



instance IsAdapter FMAdapter where
    type User FMAdapter = FMUser
    type Channel FMAdapter = FMChannel

    adapterId = "facebook-messenger"

    messageChannel = undefined

    initAdapter = return undefined

    runAdapter = undefined

    resolveChannel = const $ return Nothing

    resolveUser  = const $ return Nothing



