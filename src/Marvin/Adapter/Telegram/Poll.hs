{-# LANGUAGE FlexibleInstances #-}
module Marvin.Adapter.Telegram.Poll where


import Marvin.Adapter
import Marvin.Adapter.Telegram.Common


instance IsAdapter (TelegramAdapter Poll) where
    adapterId = "telegram-poll"

