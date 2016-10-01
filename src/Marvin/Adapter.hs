{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Marvin.Adapter where

import ClassyPrelude
import Marvin.Internal.Types
import Control.Lens
import qualified Data.Configurator.Types as C

data Event 
    = MessageEvent Message

declareFields [d|
    data OutputProvider = OutputProvider
        { outputProviderMessageRoom :: Room -> Text -> IO ()
        , outputProviderJoinRoom :: Room -> IO ()  
        , outputProviderGetUserInfo :: User -> IO (Maybe UserInfo)
        }
    |]

type EventHandler = OutputProvider -> Event -> IO ()

newtype Adapter = Adapter { adapterRunner :: EventHandler -> IO () }


declareFields [d|
    data BuildAdapter = BuildAdapter
        { buildAdapterAdapterId :: AdapterId 
        , buildAdapterBuildFunction :: C.Config -> IO Adapter 
        }
    |]