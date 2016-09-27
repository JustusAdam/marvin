module Marvin
    ( Regex, r
    , Match
    , ScriptId
    , Script, defineScript, ScriptInit
    , ScriptDefinition
    , BotReacting
    , hear, respond, send, reply
    , getScriptId, getMessage, getMatch
    , getConfigVal, getConfig
    ) where


import           Marvin.Internal
