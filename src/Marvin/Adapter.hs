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
{-# LANGUAGE FlexibleContexts #-}
module Marvin.Adapter
    ( Event(..)
    , RunWithAdapter, EventHandler, RunnerM
    , IsAdapter(..), AdapterId
    , lookupFromAdapterConfig, requireFromAdapterConfig
    , lookupFromAppConfig, requireFromAppConfig
    , getAdapterConfig, getAppConfig
    , liftAdapterAction
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Configurator.Types as C
import qualified Data.Configurator as C
import qualified Data.Text.Lazy          as L
import           Marvin.Internal.Types
import           Control.Monad.Reader
import Marvin.Interpolate.Text


liftAdapterAction :: (MonadIO m, HasConfigAccess m, AccessAdapter m, IsAdapter a, a ~ AdapterT m) => AdapterM a r -> m r
liftAdapterAction (AdapterM ac) = do
    a <- getAdapter
    c <- getConfigInternal
    liftIO $ runStderrLoggingT $ runReaderT ac (c, a)


getAppConfig :: AdapterM a C.Config
getAppConfig = AdapterM $ 
    C.subconfig $(isT "#{applicationScriptId}") . fst <$> ask


lookupFromAppConfig :: C.Configured v => C.Name -> AdapterM a (Maybe v)
lookupFromAppConfig n = getAppConfig >>= liftIO . flip C.lookup n


requireFromAppConfig :: C.Configured v => C.Name -> AdapterM a v
requireFromAppConfig n = getAppConfig >>= liftIO . flip C.require n


getAdapterConfig :: forall a. IsAdapter a => AdapterM a C.Config
getAdapterConfig = AdapterM $
    C.subconfig $(isT "adapter.#{adapterId :: AdapterId a}") . fst <$> ask


lookupFromAdapterConfig :: (IsAdapter a, C.Configured v) => C.Name -> AdapterM a (Maybe v)
lookupFromAdapterConfig n = getAdapterConfig >>= liftIO . flip C.lookup n


requireFromAdapterConfig :: (IsAdapter a, C.Configured v) => C.Name -> AdapterM a v
requireFromAdapterConfig n = getAdapterConfig >>= liftIO . flip C.require n
