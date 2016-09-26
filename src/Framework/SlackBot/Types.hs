{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Framework.SlackBot.Types where


import ClassyPrelude
import Data.Aeson
import Data.Aeson.TH
import Data.Time


newtype User = User { username :: Text } deriving (IsString, Eq, Show)
newtype Room = Room { roomname :: Text } deriving (IsString, Eq, Show)


deriveJSON defaultOptions { unwrapUnaryRecords = True } ''User
deriveJSON defaultOptions { unwrapUnaryRecords = True } ''Room


data Message = Message 
    { sender :: User 
    , channel :: Room
    , content :: Text
    , timestamp :: LocalTime 
    }
