{-|
Module      : $Header$
Description : A collection of all modules useful for using marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Prelude
    (
    -- | Common functions and Types for scripts
      module Marvin
    -- | Mutable references in marvin scripts
    , module Marvin.Util.Mutable
    -- | Logging in Scripts
    , module Control.Monad.Logger
    -- | Random numbers and convenience functions
    , module Marvin.Util.Random
    -- | Marvins regex type and how to work with it
    , module Marvin.Util.Regex
    -- | Dealing with JSON
    , module Marvin.Util.JSON
    -- | Interpolated strings a la Scala and CoffeeScript
    , module Marvin.Interpolate.String
    -- | Arbitrary IO in scripts
    , MonadIO, liftIO
    -- | Useful functions not in the normal Prelude
    , when, unless, for, for_, fromMaybe
    ) where

import           Control.Monad             (unless, when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.Foldable             (for_)
import           Data.Maybe                (fromMaybe)
import           Data.Traversable          (for)
import           Marvin
import           Marvin.Interpolate.String
import           Marvin.Util.JSON
import           Marvin.Util.Mutable
import           Marvin.Util.Random
import           Marvin.Util.Regex
