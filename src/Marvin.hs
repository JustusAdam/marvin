module Marvin 
    ( Regex, r
    , Match
    , ScriptId
    , Script, defineScript, ScriptInit
    , ScriptDefinition
    , BotReacting
    , react, respond, send
    , getScriptId, getMessage, getMatch
    , getConfigVal, getConfig
    ) where


import Marvin.Internal