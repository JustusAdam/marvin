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
    ( User(..), Channel(..), Message(..), ScriptId(..)
    , applicationScriptId, IsScript, getScriptId
    , HasConfigAccess, TimeStamp(..)
    , AdapterM()
    , AccessAdapter(AdapterT)
    , User'(User'), Channel'(Channel')
    , Get(getLens)
    ) where


import           Marvin.Internal.Types
import           Marvin.Internal.Values
