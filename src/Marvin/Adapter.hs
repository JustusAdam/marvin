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
    , IsAdapter(..), AdapterId
    , liftAdapterAction
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import qualified Data.Configurator.Types as C
import qualified Data.Text.Lazy          as L
import           Marvin.Internal.Types


liftAdapterAction :: MonadIO m => RunnerM a -> m a
liftAdapterAction ac =
    liftIO $ runStderrLoggingT ac
