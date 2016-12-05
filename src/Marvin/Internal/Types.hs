{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Marvin.Internal.Types where


import           ClassyPrelude
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char               (isAlphaNum, isLetter)
import qualified Data.Configurator.Types as C
import qualified System.Log.Logger       as L


newtype User = User Text deriving (IsString, Eq, Hashable)
newtype Room = Room Text deriving (IsString, Eq, Show, Hashable)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''User
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Room


newtype TimeStamp = TimeStamp { unwrapTimeStamp :: Double } deriving Show


data Message = Message
    { sender    :: User
    , channel   :: Room
    , content   :: String
    , timestamp :: TimeStamp
    }


instance FromJSON TimeStamp where
    parseJSON (String s) = maybe mzero (return . TimeStamp) $ readMay s
    parseJSON _          = mzero

instance ToJSON TimeStamp where
    toJSON = toJSON . show . unwrapTimeStamp


-- | A type, basically a String, which identifies a script to the config and the logging facilities.
newtype ScriptId = ScriptId { unwrapScriptId :: Text } deriving (Show, Eq)


-- | A type, basically a String, which identifies an adapter to the config and the logging facilities.
newtype AdapterId a = AdapterId { unwrapAdapterId :: Text } deriving (Show, Eq)


applicationScriptId :: ScriptId
applicationScriptId = ScriptId "bot"


verifyIdString :: String -> (String -> a) -> String -> a
verifyIdString name _ "" = error $ name ++ " must not be empty"
verifyIdString name f s@(x:xs)
    | isLetter x && all (\c -> isAlphaNum c || c == '-' || c == '_' ) xs = f s
    | otherwise = error $ "first character of " ++ name ++ " must be a letter, all other characters can be alphanumeric, '-' or '_'"


instance IsString ScriptId where
    fromString = verifyIdString "script id" (ScriptId . fromString)


instance IsString (AdapterId a) where
    fromString = verifyIdString "adapter id" (AdapterId . fromString)


class HasScriptId s a | s -> a where
    scriptId :: Lens' s a


-- | Denotes a place from which we may access the configuration.
--
-- During script definition or when handling a request we can obtain the config with 'getConfigVal' or 'requireConfigVal'.
class (IsScript m, MonadIO m) => HasConfigAccess m where
    getConfigInternal :: m C.Config


class IsScript m where
    getScriptId :: m ScriptId


prioMapping :: [(Text, L.Priority)]
prioMapping = map ((pack . show) &&& id) [L.DEBUG, L.INFO, L.NOTICE, L.WARNING, L.ERROR, L.CRITICAL, L.ALERT, L.EMERGENCY]


instance C.Configured L.Priority where
    convert (C.String s) = lookup (toUpper s) prioMapping
    convert _            = Nothing

