{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitForAll            #-}
module Marvin.Util.Config
    ( Config(Config), lookup, require, subconfig
    , Name, C.Configured(..), C.Value(..)
    , IsConfig(lookupC, subconfigC)
    ) where


import           Control.Monad.IO.Class
import qualified Data.Configurator         as C
import qualified Data.Configurator.Types   as C
import           Data.Maybe
import           Data.Text                 (Text)
import           Marvin.Interpolate.String
import           Prelude                   hiding (lookup)


type Name = Text


class IsConfig c where
    lookupC :: C.Configured v => c -> Name -> IO (Maybe v)
    subconfigC :: Name -> c -> IO c

instance IsConfig C.Config where
    lookupC = C.lookup
    subconfigC c = pure . C.subconfig c

data Config = forall c. IsConfig c => Config c


lookup :: (C.Configured v, MonadIO m) => Config -> Name -> m (Maybe v)
lookup (Config cfg) = liftIO . lookupC cfg


require :: (C.Configured v, MonadIO m) => Config -> Name -> m v
require cfg name = fromMaybe (error $(isS "Lookup error, key #{name} does not exist")) <$> lookup cfg name


subconfig :: MonadIO m => Name -> Config -> m Config
subconfig name (Config cfg) = liftIO $ Config <$> subconfigC name cfg


