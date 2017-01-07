{-# LANGUAGE FlexibleInstances #-}
module Marvin.Adapter.Telegram.Push where


import Marvin.Adapter
import Marvin.Adapter.Telegram.Common


instance IsAdapter (TelegramAdapter Push) where
    adapterId = "telegram-push"
