{-# LANGUAGE FlexibleInstances #-}
module Marvin.Adapter.Telegram.Poll where


import Marvin.Adapter
import Marvin.Adapter.Telegram.Common


data Poll


instance MkEventGetter Poll where
    getScriptId _ = "telegram-poll"
    mkEvenmkScriptId _ = error "not implemented"
