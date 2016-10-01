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
    ( User(..), UserInfo(..), Room(..), Message(..), ScriptId(..)
    , applicationScriptId, IsScript, getScriptId
    , HasConfigAccess
    ) where


import           Marvin.Internal.Types
