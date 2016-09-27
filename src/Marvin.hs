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
    , getScriptId, getMessage, getMatch
    , getConfigVal, requireConfigVal
    , BotReacting
      -- * Utilities
    , Regex, r, match
    , Match
    
    ) where


import           Marvin.Internal
