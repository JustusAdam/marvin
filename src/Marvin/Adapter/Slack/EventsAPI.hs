{-|
Module      : $Header$
Description : Adapter for communicating with Slack via the webhook based Events API
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Adapter.Slack.EventsAPI
    ( SlackAdapter, EventsAPI
    , SlackUserId, SlackChannelId
    , MkSlack
    ) where


import           Marvin.Adapter.Slack.Common
import           Marvin.Adapter.Slack.Types
import           Network.Wai
import           Network.Wai.Handler.Warp


-- | Recieve events as a server via HTTP webhook (not implemented yet)
data EventsAPI


instance MkSlack EventsAPI where
    mkAdapterId = "slack-events"
    initIOConnections = error "not implemented"
