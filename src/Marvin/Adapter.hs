{-# LANGUAGE FlexibleInstances #-}
module Marvin.Adapter where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Configurator.Types as C
import           Marvin.Internal.Types

data Event
    = MessageEvent Message

type EventHandler a = a -> Event -> IO ()


class IsAdapter a where
    adapterId :: AdapterId a
    messageRoom :: a -> Room -> Text -> IO ()
    getUserInfo :: a -> User -> IO (Maybe UserInfo)
    runWithAdapter :: RunWithAdapter a


type RunWithAdapter a = C.Config -> EventHandler a -> IO ()
