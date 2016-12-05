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


import Data.IORef
import Control.Monad.IO.Class

-- | A mutable reference to a value of type @v@
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


-- type Synchronized = MVar


-- readSynchronized :: MonadIO m => Synchronized a -> m a
-- readSynchronized = liftIO . readMVar


-- tryReadSynchronized :: MonadIO m => Synchronized a -> m (Maybe a)
-- tryReadSynchronized = liftIO . tryReadMVar


-- writeSynchronized :: MonadIO m => Synchronized a -> m ()
-- writeSynchronized
