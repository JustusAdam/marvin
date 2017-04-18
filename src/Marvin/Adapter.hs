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
{-# LANGUAGE ScopedTypeVariables #-}
module Marvin.Adapter
    ( EventConsumer
    , IsAdapter(..), AdapterId, mkAdapterId, unwrapAdapterId
    , AdapterM
    , HasFiles(..)
    , Event(..)
    , lookupFromAdapterConfig, requireFromAdapterConfig
    , lookupFromAppConfig, requireFromAppConfig, getBotname
    , getAdapterConfig, getAppConfig, getAdapter
    , liftAdapterAction
    , HasAdapter(adapter), HasConfig(config)
    ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Configurator           as C
import qualified Data.Configurator.Types     as C
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text.Lazy              as L
import           Marvin.Internal.LensClasses
import           Marvin.Internal.Types
import           Marvin.Internal.Values
import           Marvin.Interpolate.Text


liftAdapterAction :: (MonadIO m, HasConfigAccess m, AccessAdapter m, IsAdapter a, a ~ AdapterT m) => AdapterM a r -> m r
liftAdapterAction (AdapterM ac) = do
    a <- getAdapter
    c <- getConfigInternal
    liftIO $ runStderrLoggingT $ runReaderT ac (AdapterMEnv c a)


getAppConfig :: AdapterM a C.Config
getAppConfig = AdapterM $
    C.subconfig $(isT "#{applicationScriptId}") <$> view config


lookupFromAppConfig :: C.Configured v => C.Name -> AdapterM a (Maybe v)
lookupFromAppConfig n = getAppConfig >>= liftIO . flip C.lookup n


requireFromAppConfig :: C.Configured v => C.Name -> AdapterM a v
requireFromAppConfig n = getAppConfig >>= liftIO . flip C.require n


getBotname :: AdapterM a L.Text
getBotname = fromMaybe defaultBotName <$> lookupFromAppConfig "name"


getAdapterConfig :: forall a. IsAdapter a => AdapterM a C.Config
getAdapterConfig = AdapterM $
    C.subconfig $(isT "#{adapterConfigKey}.#{adapterId :: AdapterId a}") <$> view config


lookupFromAdapterConfig :: (IsAdapter a, C.Configured v) => C.Name -> AdapterM a (Maybe v)
lookupFromAdapterConfig n = getAdapterConfig >>= liftIO . flip C.lookup n


requireFromAdapterConfig :: (IsAdapter a, C.Configured v) => C.Name -> AdapterM a v
requireFromAdapterConfig n = getAdapterConfig >>= liftIO . flip C.require n
