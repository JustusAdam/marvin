{-|
Module      : $Header$
Description : Random numbers and utility functions
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Util.Random
    ( randomVal, randomValFromRange, randomFrom
    , module System.Random
    ) where


import           Control.Monad.IO.Class
import qualified Data.List.NonEmpty     as NE
import           System.Random


-- | Generate a random value. For more information see 'random'
randomVal :: (MonadIO m, Random a) => m a
randomVal = liftIO randomIO


-- | Generate a random value bounded by a range. See 'randomR' for how the range works.
randomValFromRange :: (MonadIO m, Random a) => (a, a) -> m a
randomValFromRange = liftIO . randomRIO


-- | Get a random value out of a list of values.
-- This uses 'length' and therefore does not terminate for infinite lists.
--
-- The list must be non empty (hence the 'NE.NonEmpty' type)
-- however using the @-XOverloadedLists@ extension you can write
-- normal list literals for this using the @[elem1, elem2]@ syntax
--
-- Uses the global random number generator.
--
-- Usable in all IO capable monads, such as 'BotReacting' and 'ScriptDefinition'.
randomFrom :: MonadIO m => NE.NonEmpty e -> m e
randomFrom list = (list NE.!!) <$> randomValFromRange (0, pred $ NE.length list)
