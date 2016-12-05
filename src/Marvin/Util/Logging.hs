{-|
Module      : $Header$
Description : Logging facilities for marvin scripts.
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
module Marvin.Util.Logging
    ( debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM, logM
    ) where

import           Marvin.Types
import qualified System.Log.Logger as L
import Control.Monad.IO.Class
import Data.Text (unpack)

scriptLog :: (MonadIO m, IsScript m) => (String -> String -> IO ()) -> String -> m ()
scriptLog inner message = do
    (ScriptId sid) <- getScriptId
    liftIO $ inner ("script." ++ unpack sid) message


debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM :: (MonadIO m, IsScript m) => String -> m ()
debugM = scriptLog L.debugM
infoM = scriptLog L.infoM
noticeM = scriptLog L.noticeM
warningM = scriptLog L.warningM
errorM = scriptLog L.errorM
criticalM = scriptLog L.criticalM
alertM = scriptLog L.alertM
emergencyM = scriptLog L.emergencyM


logM :: (MonadIO m, IsScript m) => L.Priority -> String -> m ()
logM prio = scriptLog (`L.logM` prio)
