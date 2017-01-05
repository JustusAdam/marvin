{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Marvin.Internal.Types where


import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char               (isAlphaNum, isLetter)
import qualified Data.Configurator.Types as C
import           Data.Hashable
import           Data.String
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import           Marvin.Interpolate.Text
import qualified Data.HashMap.Strict as HM
import Text.Read (readMaybe)


-- | Identifier for a user (internal and not necessarily equal to the username)
newtype User = User T.Text deriving (IsString, Eq, Hashable)
-- | Identifier for a channel (internal and not necessarily equal to the channel name)
newtype Channel = Channel T.Text deriving (IsString, Eq, Show, Hashable)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''User
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Channel


newtype TimeStamp = TimeStamp { unwrapTimeStamp :: Double } deriving Show


-- | contents and meta information of a recieved message
data Message = Message
    { sender    :: User
    , channel   :: Channel
    , content   :: LT.Text
    , timestamp :: TimeStamp
    }


instance FromJSON TimeStamp where
    parseJSON (String s) = maybe mzero (return . TimeStamp) $ readMaybe (T.unpack s)
    parseJSON _          = mzero

instance ToJSON TimeStamp where
    toJSON = toJSON . show . unwrapTimeStamp


-- | A type, basically a String, which identifies a script to the config and the logging facilities.
newtype ScriptId = ScriptId { unwrapScriptId :: T.Text } deriving (Show, Eq)


-- | A type, basically a String, which identifies an adapter to the config and the logging facilities.
newtype AdapterId a = AdapterId { unwrapAdapterId :: T.Text } deriving (Show, Eq)


instance ShowT ScriptId where showT = unwrapScriptId

instance ShowT (AdapterId a) where showT = unwrapAdapterId


applicationScriptId :: ScriptId
applicationScriptId = ScriptId "bot"


type RunnerM = LoggingT IO


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

instance C.Configured LogLevel where
    convert (C.String s) = 
        case T.strip $ T.toLower s of
            "debug" -> Just LevelDebug
            "warning" -> Just LevelWarn
            "error" -> Just LevelError
            "info" -> Just LevelInfo
            _ -> Nothing
    convert _            = Nothing

