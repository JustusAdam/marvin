{-|
Module      : $Header$
Description : Marvin the modular bot
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

For the proper, verbose documentation see <https://marvin.readthedocs.org/en/latest/scripting.html>.

-}
module Marvin
    (
    -- * The Script
      Script(..), defineScript, ScriptInit
    , ScriptId
    , ScriptDefinition, IsAdapter
    -- * Reacting
    -- ** Reaction Functions
    , hear, respond, enter, exit, enterIn, exitFrom, topic, topicIn, customTrigger
    -- ** Getting data
    , getData, getMessage, getMatch, getTopic, getChannel, getUser, getUsername, getChannelName, resolveChannel
    -- ** Sending messages
    , send, reply, messageChannel, messageChannel'
    -- ** Interaction with the config
    , getConfigVal, requireConfigVal, getBotName
    -- ** Handler Types
    , Message(..), User, Channel, BotReacting
    -- ** Advanced actions
    , extractAction, extractReaction
    -- ** Misc
    , Topic
    ) where


import           Marvin.Adapter (IsAdapter)
import           Marvin.Internal
import           Marvin.Internal.Types hiding (getChannelName, getUsername, messageChannel)
