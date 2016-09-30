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
        }
    |]

type EventHandler = Event -> IO ()


declareFields [d|
    data Adapter = Adapter
        { adapterRunner :: EventHandler -> IO () 
        , adapterOutputProvider :: OutputProvider
        }
    |]


declareFields [d|
    data BuildAdapter = BuildAdapter
        { buildAdapterAdapterId :: AdapterId 
        , buildAdapterBuildFunction :: C.Config -> IO Adapter 
        }
    |]