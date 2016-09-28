{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Marvin.Types
    ( User(..), Room(..), Message(..), ScriptId(..)
    , applicationScriptId, IsScript, getScriptId
    ) where


import           ClassyPrelude
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char               (isAlphaNum, isLetter)
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C
import           Data.Time
import qualified System.Log.Logger       as L


newtype User = User { username :: Text } deriving (IsString, Eq, Show)
newtype Room = Room { roomname :: Text } deriving (IsString, Eq, Show)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''User
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Room


data Message = Message
    { sender    :: User
    , channel   :: Room
    , content   :: Text
    , timestamp :: LocalTime
    }



-- | A type, basically a String, which identifies a script to the config and the logging facilities.
newtype ScriptId = ScriptId { unwrapScriptId :: Text } deriving (Show, Eq)


applicationScriptId :: ScriptId
applicationScriptId = ScriptId "bot"


instance IsString ScriptId where
    fromString "" = error "script id must not be empty"
    fromString "bot" = error "'bot' is a protected name and cannot be used as script id"
    fromString s@(x:xs) =
        if isLetter x && all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs
            then ScriptId $ fromString s
            else error "first character of script id must be a letter, all other characters can be alphanumeric, '-' or '_'"


class IsScript m where
    getScriptId :: m ScriptId


-- | Denotes a place from which we may access the configuration.
--
-- During script definition or when handling a request we can obtain the config with 'getConfigVal' or 'requireConfigVal'.
class (IsScript m, MonadIO m) => HasConfigAccess m where
    getConfigInternal :: m C.Config


prioMapping = map ((pack . show) &&& id) [L.DEBUG, L.INFO, L.NOTICE, L.WARNING, L.ERROR, L.CRITICAL, L.ALERT, L.EMERGENCY]


instance C.Configured L.Priority where
    convert (C.String s) = lookup (toUpper s) prioMapping
    convert _ = Nothing
