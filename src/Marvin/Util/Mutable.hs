{-|
Module      : $Header$
Description : Mutable references for marvin.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Util.Mutable where


import           Control.Monad.IO.Class
import           Data.IORef
import Control.Concurrent.MVar

-- | A mutable reference to a value of type @v@
--
-- This is like a pointer in c, the value behind which can be mutated by several functions. 
-- So long as they retain this reference they are able to retrieve the updated value.
type Mutable v = IORef v


-- | Create a new mutable reference of type @v@ from an initial value.
newMutable :: MonadIO m => a -> m (Mutable a)
newMutable = liftIO . newIORef


-- | Retrieve the value behind by a mutable reference.
readMutable :: MonadIO m => Mutable a -> m a
readMutable = liftIO . readIORef


-- | Set the value inside a mutable reference.
writeMutable :: MonadIO m => Mutable a -> a -> m ()
writeMutable m = liftIO . writeIORef m


-- | Change the value behind a mutable reference.
modifyMutable :: MonadIO m => Mutable a -> (a -> a) -> m ()
modifyMutable m = liftIO . modifyIORef m


-- | A value that can be shared on multiple concurrent Threads.
-- 
-- This value works like a channel. It can either be empty or full.
-- If it is empty 'writeSynchronized' fills it, otherwise the write blocks.
-- If it is full 'takeSynchronized' empties, otherwise it blocks until it is filled.
-- 'readSynchronized' does not empty it and also blocks if the 'Synchronized' is empty.
--
-- Should you just use it as a thread safe mutable variable, mutations typically follow the pattern:
-- @
--   val <- takeSynchronized -- obtain the value and leave it empty to block concurrent reads
--   let mod = modify val -- modify the value
--   writeSynchronized val -- write back the result
-- @
--
-- Another use for this type is as a message channel, where we have a producer and a consumer, 
-- the producer tries to write values into the 'Synchronized' ('writeSynchronized') and the consumer
-- waits for the 'Synchronized' to be filled and takes the value 'takeSynchronized' for procesing.
--
-- It works generally best if any 'Synchronized' is only used for one of these two applications at the same time.
--
-- This type is the same as 'MVar', only renamed for readability. If you want a more in depth documentation, see the documentation for 'MVar'.
type Synchronized = MVar


-- | Read the vaue, but don't empty the 'Synchronized'. Blocks if it is empty.
readSynchronized :: MonadIO m => Synchronized a -> m a
readSynchronized = liftIO . readMVar


-- | Non blocking version of 'readSynchronized'. Returns 'Nothing' if it was empty.
tryReadSynchronized :: MonadIO m => Synchronized a -> m (Maybe a)
tryReadSynchronized = liftIO . tryReadMVar


-- | Read the value and empty the 'Synchronized', blocks if already empty.
takeSynchronized :: MonadIO m => Synchronized a -> m a
takeSynchronized = liftIO . takeMVar


-- | Non blocking version of 'takeSynchronized', returns 'Nothing' if it was empty.
tryTakeSynchronized :: MonadIO m => Synchronized a -> m (Maybe a)
tryTakeSynchronized = liftIO . tryTakeMVar


-- | Fills the empty 'Synchronized' with a value, blocks if it is full.
writeSynchronized :: MonadIO m => Synchronized a -> a -> m ()
writeSynchronized mv = liftIO . putMVar mv


-- | Non blocking version of 'writeSynchronized'. Returns 'False' if it was full.
tryWriteSynchronized :: MonadIO m => Synchronized a -> a -> m Bool
tryWriteSynchronized mv = liftIO . tryPutMVar mv


-- | Query if the 'Synchronized' is empty or full.
isEmptySynchronized :: MonadIO m => Synchronized a -> m Bool
isEmptySynchronized = liftIO . isEmptyMVar
