{-|
Module      : $Header$
Description : Adapter for communicating with Telegram via its http poll API.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX


=== Caveats:

'resolveUser' and 'resolveChannel' resolving are not yet supported in this adapter and always returns 'Nothing'. See <https://github.com/JustusAdam/marvin/issues/10 #10>.
-}
{-# LANGUAGE CPP #-}
module Marvin.Adapter.Telegram.Poll
    ( TelegramAdapter, Poll
    , TelegramChat(..), ChatType(..)
    , TelegramUser(..)
    , MkTelegram
    , HasUsername(username), HasLastName(lastName), HasId_(id_), HasFirstName(firstName), HasType_(type_)
    ) where



import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Data.Aeson                     hiding (Error, Success)
import           Data.Aeson.Types               hiding (Error, Success)
import           Data.IORef.Lifted
import           Marvin.Adapter
import           Marvin.Adapter.Telegram.Common
import           Marvin.Interpolate.Text
import           Network.HTTP.Client            (managerResponseTimeout)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Network.Wreq

#if MIN_VERSION_http_client(0,5,0)
import           Network.HTTP.Client            (responseTimeoutMicro)
#else
responseTimeoutMicro = Just
#endif

data UpdateWithId = UpdateWithId {updateId :: Integer, updateContent :: TelegramUpdate Poll }

instance FromJSON UpdateWithId where
    parseJSON = withObject "expected object" $ \o -> UpdateWithId <$> o .: "update_id" <*> parseJSON (Object o)

pollEventGetter :: Chan (TelegramUpdate Poll) -> AdapterM (TelegramAdapter Poll) ()
pollEventGetter msgChan = do
    idRef <- newIORef Nothing
    forever $ do
        timeout <- lookupFromAdapterConfig "polling-timeout" >>= readTimeout
        let defParams = ["timeout" := (timeout :: Int) ]
        nextId <- readIORef idRef
        let pollSettings = defaults & manager . _Left .~ tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro ((timeout + 3) * 1000)}
        response <- execAPIMethodWith pollSettings parseJSON "getUpdates" $ maybe defParams ((:defParams) . ("offset" :=)) nextId
        case response of
            Left err -> do
                logErrorN $(isT "Unable to parse json: #{err}")
                threadDelay 30000
            Right (Error code desc) -> do
                logErrorN $(isT "Sending message failed with #{code}: #{desc}")
                threadDelay 30000
            Right Success {result=[]} -> return ()
            Right Success {result=updates} -> do
                writeIORef idRef $ Just $ succ $ maximum $ map updateId updates
                logDebugN "Writing messages"
                writeList2Chan msgChan $ map updateContent updates
  where
    defaultTimeout = 120
    readTimeout Nothing = return defaultTimeout
    readTimeout (Just n)
        | n < 0 = do
            logErrorN $(isT "Telegram adapter poll timeout must be positive, was #{n} (using default timeout instead)")
            return defaultTimeout
        | otherwise = return n


-- | Use the telegram API by fetching updates via HTTP
data Poll


instance MkTelegram Poll where
    mkAdapterId = "telegram-poll"
    mkEventGetter = pollEventGetter
