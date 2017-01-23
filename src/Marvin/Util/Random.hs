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
import           System.Random


-- | Generate a random value. For more information see 'random'
randomVal :: (MonadIO m, Random a) => m a
randomVal = liftIO randomIO


-- | Generate a random value frbounded by a range. See 'randomR' for how the range works.
randomValFromRange :: (MonadIO m, Random a) => (a, a) -> m a
randomValFromRange = liftIO . randomRIO


-- | Get a random value out of an integer indexed sequence, like ('Vector', '[a]', 'Seq' or 'Set')
-- This uses the sequences 'length' and therefore does not terminate for infinite sequences.
--
-- Uses the global random number generator.
--
-- Usable in all IO capable monads, such as 'BotReacting' and 'ScriptDefinition'.
randomFrom :: MonadIO m => [e] -> m e
randomFrom list = do
  n <- randomValFromRange (0, pred $ length list)
  return $ list !! n
