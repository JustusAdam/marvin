{-|
Module      : $Header$
Description : Adapter for communicating with Telegram via its webhook based push API.
Copyright   : (c) Justus Adam, 2017
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Adapter.Telegram.Push
    ( TelegramAdapter, Push
    , TelegramChat(..), ChatType(..)
    , TelegramUser(..)
    , MkTelegram
    ) where


import           Control.Concurrent.Chan.Lifted
import           Marvin.Adapter
import           Marvin.Adapter.Telegram.Common



pushEventGetter :: Chan (TelegramUpdate Push) -> AdapterM (TelegramAdapter Push) ()
pushEventGetter msgChan =
    -- port <- liftIO $ C.require cfg "port"
    -- url <- liftIO $ C.require cfg "url"
    error "not implemented"


-- | Use the telegram API by recieving updates as a server via webhook
--
-- Note: The initialization for this adapter _includes_ registering or clearing its own webhook.
data Push


instance MkTelegram Push where
    mkAdapterId = "telegram-push"
    mkEventGetter = pushEventGetter
