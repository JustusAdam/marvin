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
    -- ** Common functions and Types for scripts
      module Marvin
    -- ** Logging in Scripts
    , module Control.Monad.Logger
    -- ** Random numbers and convenience functions
    , module Marvin.Util.Random
    -- ** Marvins regex type and how to work with it
    , module Marvin.Util.Regex
    -- ** Dealing with JSON
    , module Marvin.Util.JSON
    -- ** Interpolated strings a la Scala and CoffeeScript
    , isL, isT, isS
    -- ** Arbitrary IO in scripts
    , MonadIO, liftIO
    -- ** Lenses
    , module Control.Lens
    -- ** Useful functions not in the normal Prelude
    , when, unless, for, for_, filterM, fromMaybe
    ) where

import           Control.Lens                 (lens, (&), (.~), (^.), at, ix, (^?), (^?!))
import           Control.Monad                (unless, when, filterM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Logger
import           Data.Foldable                (for_)
import           Data.Maybe                   (fromMaybe)
import           Data.Traversable             (for)
import           Marvin
import           Marvin.Interpolate.String    (isS)
import           Marvin.Interpolate.Text      (isT)
import           Marvin.Interpolate.Text.Lazy (isL)
import           Marvin.Util.JSON
import           Marvin.Util.Random
import           Marvin.Util.Regex
