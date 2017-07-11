{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
module Marvin.Util.Config
    ( Config(Config), lookup, require, subconfig, IsConfig(lookupIO, subconfigIO)
    , Name, C.Configured(..), C.Value(..)
    ) where


import           Control.Monad.IO.Class
import qualified Data.Configurator         as C
import qualified Data.Configurator.Types   as C
import           Data.Maybe
import           Data.Text                 (Text)
import           Marvin.Interpolate.String
import           Prelude                   hiding (lookup)


-- | Name of a value on the config
type Name = Text


-- | Constraint for an abstract config
class IsConfig c where
    -- | Retrieve a value from a config.
    -- Returns 'Nothing' if the key is not present or cannot be converted to @v@.
    lookupIO :: C.Configured v => c -> Name -> IO (Maybe v)
    -- | Obtain a slice of a config pertaining to a certain 'Name'
    subconfigIO :: Name -> c -> IO c

instance IsConfig C.Config where
    lookupIO = C.lookup
    subconfigIO c = pure . C.subconfig c

instance IsConfig Config where
    lookupIO (Config c) = lookupIO c
    subconfigIO name (Config c) = Config <$> subconfigIO name c


-- | Exietential wrapper for configs to capture the 'Config' typeclass and therefore simplify type signatures.
data Config = forall c. IsConfig c => Config c


-- | Obtain a value from a config. 
-- Throws an error if the value is not present or cannot be converted to @v@.
require :: (C.Configured v, MonadIO m, IsConfig c) => c -> Name -> m v
require cfg name = liftIO $ fromMaybe (error $(isS "Lookup error, key #{name} does not exist")) <$> lookup cfg name


-- | Retrieve a value from a config.
-- Returns 'Nothing' if the key is not present or cannot be converted to @v@.
lookup :: (MonadIO m, IsConfig c, C.Configured v) => c -> Name -> m (Maybe v)
lookup cfg = liftIO . lookupIO cfg


-- | Obtain a slice of a config pertaining to a certain 'Name'
subconfig :: (MonadIO m, IsConfig c) => Name -> c -> m c
subconfig name = liftIO . subconfigIO name
