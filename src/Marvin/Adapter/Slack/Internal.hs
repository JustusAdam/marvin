{-|
Module      : $Header$
Description : Internal types for the slack adapter
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX

See http://marvin.readthedocs.io/en/latest/adapters.html#slack for documentation about this adapter.

The contents of this module are intended to provide access to internal functions and data structures for the slack adaper.
The use of this module is intended for advanced users.
No part of the API exposed here is to be consideres stable and may change unexpectedly.
-}
module Marvin.Adapter.Slack.Internal
    ( SlackUserId(..), SlackChannelId(..)
    , MkSlack(..), SlackAdapter(..), InternalType(..)
    , LimitedChannelInfo(..), ChannelCache(..), UserCache(..)
    , UserInfo(..)
    , HasTopic(..), HasIdValue(..), HasNameResolver(..), HasInfoCache(..), HasCreated(..)
    ) where


import           Marvin.Adapter.Slack.Internal.Common
import           Marvin.Adapter.Slack.Internal.Types
