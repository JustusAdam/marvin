{-|
Module      : $Header$
Description : Marvin the modular bot
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

-}
module Marvin
    ( -- * Scripts
      Script(..), defineScript, ScriptInit
    , ScriptId
    , ScriptDefinition
    , IsAdapter
      -- * Reacting
    , hear, respond, customTrigger, send, reply, messageChannel
    , getData, getMessage, getMatch, getUsername, getChannelName
    , Message(..), User, Channel
    , getConfigVal, requireConfigVal
    , BotReacting, HasMessage(messageLens), HasMatch(matchLens)
    -- ** Advanced actions
    , extractAction, extractReaction
    ) where


import           Marvin.Adapter  (IsAdapter)
import           Marvin.Internal
import           Marvin.Internal.Types
import           Marvin.Types
