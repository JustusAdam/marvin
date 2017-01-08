{-# LANGUAGE FlexibleInstances #-}
module Marvin.Adapter.Telegram.Push where


import Marvin.Adapter
import Marvin.Adapter.Telegram.Common



data Push


instance MkEventGetter Push where
    mkScriptId _ = "telegram-push"
    mkEventGetter _ = error "not implemented"
