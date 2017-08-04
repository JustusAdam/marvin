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
    (
    -- * Types
      EventConsumer
    , IsAdapter(..), AdapterId, mkAdapterId, unwrapAdapterId
    , AdapterM
    , SupportsFiles(..)
    , MonadAdapter(..)
    , Event(..)
    -- * Interacting with the config
    -- ** For the adapter
    , lookupFromAdapterConfig, requireFromAdapterConfig, getAdapterConfig
    -- ** For the application
    -- | usually adapter should not have to retrieve values from the application config
    -- therefore use this functionality with caution.
    -- The structure of the application config is not guaranteed to remain consistent across versions.
    --
    -- Prefer using helper functions such as 'getBotname' which will remain stable across versions.
    , lookupFromAppConfig, requireFromAppConfig, getAppConfig, getBotname
    -- * Misc
    , getAdapterId, getAdapterIdM, getAdapter
    -- * Lenses
    , HasAdapter(adapter), HasConfig(config)
    ) where

import           Control.Monad.IO.Class
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text.Lazy              as L
import           Lens.Micro.Platform
import           Marvin.Internal.LensClasses
import           Marvin.Internal.Types
import           Marvin.Internal.Values
import           Marvin.Interpolate.Text
import qualified Marvin.Util.Config          as C


-- | Obtain the application config structure
getAppConfig :: AdapterM a C.Config
getAppConfig = AdapterM $
    C.subconfig $(isT "#{applicationScriptId}") =<< view config


-- | Get a value from the application config.
-- Returns 'Nothing' if the value does not exist or cannot be converted to @a@.
lookupFromAppConfig :: C.Configured v => C.Name -> AdapterM a (Maybe v)
lookupFromAppConfig n = getAppConfig >>= liftIO . flip C.lookup n


-- | Get a value from the application config.
-- Throws an error if the value does not exist or cannot be converted to @a@.
requireFromAppConfig :: C.Configured v => C.Name -> AdapterM a v
requireFromAppConfig n = getAppConfig >>= liftIO . flip C.require n


-- | Convenience function to get the name of the bot from the config
getBotname :: AdapterM a L.Text
getBotname = fromMaybe defaultBotName <$> lookupFromAppConfig "name"


-- | Obtain the configuration structure for this adapter
getAdapterConfig :: forall a. IsAdapter a => AdapterM a C.Config
getAdapterConfig = AdapterM $
    C.subconfig $(isT "#{adapterConfigKey}.#{adapterId :: AdapterId a}") =<< view config


-- | Get a value from the adapter config.
-- Returns 'Nothing' if the value does not exist or cannot be converted to @a@.
lookupFromAdapterConfig :: (IsAdapter a, C.Configured v) => C.Name -> AdapterM a (Maybe v)
lookupFromAdapterConfig n = getAdapterConfig >>= liftIO . flip C.lookup n


-- | Get a value from the adapter config.
-- Throws an error if the value does not exist or cannot be converted to @a@.
requireFromAdapterConfig :: (IsAdapter a, C.Configured v) => C.Name -> AdapterM a v
requireFromAdapterConfig n = getAdapterConfig >>= liftIO . flip C.require n


-- | Get the adapter from inside the adapter monad.
getAdapter :: AdapterM a a
getAdapter = AdapterM $ view adapter


-- | Convenience function to obtain an 'AdapterId' from the adapter (this frees you from using @ScopedTypeVariables@)
getAdapterId :: IsAdapter a => a -> AdapterId a
getAdapterId _ = adapterId


-- | Convenience function to obtain an 'AdapterId' from the adapter monad (this frees you from using @ScopedTypeVariables@)
getAdapterIdM :: IsAdapter a => AdapterM a (AdapterId a)
getAdapterIdM = return adapterId
