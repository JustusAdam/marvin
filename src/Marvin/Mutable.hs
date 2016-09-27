module Marvin.Mutable where


import           ClassyPrelude


type Mutable = IORef


newMutable :: MonadIO m => a -> m (Mutable a)
newMutable = liftIO . newIORef


readMutable :: MonadIO m => Mutable a -> m a
readMutable = liftIO . readIORef


writeMutable :: MonadIO m => Mutable a -> a -> m ()
writeMutable m = liftIO . writeIORef m


modifyMutable :: MonadIO m => Mutable a -> (a -> a) -> m ()
modifyMutable m = liftIO . modifyIORef m


-- type Synchronized = MVar


-- readSynchronized :: MonadIO m => Synchronized a -> m a
-- readSynchronized = liftIO . readMVar


-- tryReadSynchronized :: MonadIO m => Synchronized a -> m (Maybe a)
-- tryReadSynchronized = liftIO . tryReadMVar


-- writeSynchronized :: MonadIO m => Synchronized a -> m ()
-- writeSynchronized
