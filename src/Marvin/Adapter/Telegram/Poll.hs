{-|
Module      : $Header$
Description : Adapter for communicating with Telegram via its http poll API.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Adapter.Telegram.Poll
    ( TelegramAdapter, Poll
    , TelegramChat(..), ChatType(..)
    , TelegramUser(..)
    , MkTelegram
    ) where



import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.Lifted
import           Control.Monad
import           Control.Monad.Logger
import           Data.Aeson                     hiding (Error, Success)
import           Marvin.Adapter
import           Marvin.Adapter.Telegram.Common
import           Marvin.Interpolate.Text



pollEventGetter :: Chan (TelegramUpdate Poll) -> AdapterM (TelegramAdapter Poll) ()
pollEventGetter msgChan =
    forever $ do
        response <- execAPIMethod parseJSON "getUpdates" []
        case response of
            Left err -> do
                logErrorN $(isT "Unable to parse json: #{err}")
                threadDelay 30000
            Right (Error code desc) -> do
                logErrorN $(isT "Sending message failed with #{code}: #{desc}")
                threadDelay 30000
            Right Success {result=updates} ->
                writeList2Chan msgChan updates

-- | Use the telegram API by fetching updates via HTTP
data Poll


instance MkTelegram Poll where
    mkAdapterId = "telegram-poll"
    mkEventGetter = pollEventGetter
