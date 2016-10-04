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
    -- | For exposing parameterised and generalised versions of prelude functions
    module ClassyPrelude
    -- | Marvin :3
    --
    -- Common functions and Types for scripts
    , module Marvin
    -- | Mutable references in marvin scripts
    , module Marvin.Util.Mutable
    -- | Logging in Scripts
    , module Marvin.Util.Logging
    -- | Random numbers and convenience functions
    , module Marvin.Util.Random
    -- | Marvins regex type and how to work with it
    , module Marvin.Util.Regex
    -- | Format strings which resolve to efficient Strings, aka 'Text'
    , module Data.Text.Format
    ) where

import           ClassyPrelude
import           Data.Text.Format (format)
import           Marvin
import           Marvin.Util.Mutable
import Marvin.Util.Regex
import Marvin.Util.Random
import Marvin.Util.Logging
