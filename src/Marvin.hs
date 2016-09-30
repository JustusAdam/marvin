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
      Script, defineScript, ScriptInit
    , ScriptId
    , ScriptDefinition
      -- * Reacting
    , hear, respond, send, reply, messageRoom
    , getMessage, getMatch
    , getConfigVal, requireConfigVal
    , BotReacting
    , MessageReactionData, messageField, matchField
      -- * Utilities
    , Regex, r, match
    , Match
      -- * Logging
    , debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM
    ) where


import           Marvin.Internal hiding (match)
import           Marvin.Logging
import           Marvin.Regex
import           Marvin.Types
