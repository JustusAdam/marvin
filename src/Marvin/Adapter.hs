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
    , RunWithAdapter, EventHandler, InitEventHandler
    , IsAdapter(..)
    , debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM, logM
    ) where

import           Control.Monad.IO.Class
import qualified Data.Configurator.Types as C
import           Data.Text               (unpack)
import           Marvin.Internal.Types
import qualified System.Log.Logger       as L


-- | Representation for the types of events which can occur
data Event
    = MessageEvent Message


type EventHandler a = Event -> IO ()
type InitEventHandler a = a -> IO (EventHandler a)


-- | Basic functionality required of any adapter
class IsAdapter a where
    -- | Used for scopting config and logging
    adapterId :: AdapterId a
    -- | Post a message to a channel given the internal channel identifier
    messageChannel :: a -> Channel -> String -> IO ()
    -- | Initialize and run the bot
    runWithAdapter :: RunWithAdapter a
    -- | Resolve a username given the internal user identifier
    getUsername :: a -> User -> IO String
    -- | Resolve the human readable name for a channel given the  internal channel identifier
    getChannelName :: a -> Channel -> IO String
    -- | Resolve to the internal channel identifier given a human readable name
    resolveChannel :: a -> String -> IO (Maybe Channel)


type RunWithAdapter a = C.Config -> InitEventHandler a -> IO ()



adapterLog :: forall m a. (MonadIO m, IsAdapter a) => (String -> String -> IO ()) -> a -> String -> m ()
adapterLog inner _ message =
    liftIO $ inner ("adapter." ++ unpack aid) message
  where (AdapterId aid) = adapterId :: AdapterId a


-- | Logging functions for adapters
debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM :: (MonadIO m, IsAdapter a) => a -> String -> m ()
debugM = adapterLog L.debugM
infoM = adapterLog L.infoM
noticeM = adapterLog L.noticeM
warningM = adapterLog L.warningM
errorM = adapterLog L.errorM
criticalM = adapterLog L.criticalM
alertM = adapterLog L.alertM
emergencyM = adapterLog L.emergencyM


logM :: (MonadIO m, IsAdapter a) => L.Priority -> a -> String -> m ()
logM prio = adapterLog (`L.logM` prio)
