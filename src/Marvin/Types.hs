{-# OPTIONS_HADDOCK not-home #-}
{-|
Module      : $Header$
Description : Common types in marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Types
    ( User(..), Channel(..), Message(..), Script
    , ScriptId, mkScriptId, unwrapScriptId
    , applicationScriptId, IsScript, getScriptId
    , HasConfigAccess, TimeStamp(..)
    , AccessAdapter(AdapterT)
    , User'(..), Channel'(..), RemoteFile'(..), FileContent
    , Get(getLens)
    , Event(..), RunnerM
    , BotActionState
    , HasScriptId(scriptId), HasAdapter(adapter), HasPayload(payload)
    , HasActions(actions), HasUsername(username), HasName(name), HasFirstName(firstName), HasLastName(lastName), HasType_(type_), HasUrl(url), HasCreationDate(creationDate), HasSize(size), HasContent(content)
    ) where


import           Marvin.Internal.Types
import           Marvin.Internal.Values
