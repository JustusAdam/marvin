{-|
Module      : $Header$
Description : The adapter interface
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Marvin.Adapter
    ( Event(..)
    , RunWithAdapter, EventHandler, InitEventHandler, RunnerM
    , IsAdapter(..)
    , liftAdapterAction
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy          as L
import           Marvin.Internal.Types


-- | Representation for the types of events which can occur
data Event
    = MessageEvent Message
    | ChannelJoinEvent User Channel
    | ChannelLeaveEvent User Channel
    | TopicChangeEvent L.Text Channel


type EventHandler a = Event -> IO ()
type InitEventHandler a = a -> IO (EventHandler a)
type RunWithAdapter a = C.Config -> InitEventHandler a -> RunnerM ()


-- | Basic functionality required of any adapter
class IsAdapter a where
    -- | Used for scoping config and logging
    adapterId :: AdapterId a
    -- | Post a message to a channel given the internal channel identifier
    messageChannel :: a -> Channel -> L.Text -> RunnerM ()
    -- | Initialize and run the bot
    runWithAdapter :: RunWithAdapter a
    -- | Resolve a username given the internal user identifier
    getUsername :: a -> User -> RunnerM L.Text
    -- | Resolve the human readable name for a channel given the  internal channel identifier
    getChannelName :: a -> Channel -> RunnerM L.Text
    -- | Resolve to the internal channel identifier given a human readable name
    resolveChannel :: a -> L.Text -> RunnerM (Maybe Channel)


liftAdapterAction :: MonadIO m => RunnerM a -> m a
liftAdapterAction = liftIO . runStderrLoggingT
