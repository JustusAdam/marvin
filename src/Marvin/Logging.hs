module Marvin.Logging where

import           ClassyPrelude
import           Marvin.Types
import qualified System.Log.Logger as L


scriptLog :: (MonadIO m, HasConfigAccess m) => (String -> String -> IO ()) -> Text -> m ()
scriptLog inner message = do
    (ScriptId sid) <- getScriptId
    liftIO $ inner (unpack $ "script." ++ sid) (unpack message)


debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM :: (MonadIO m, HasConfigAccess m) => Text -> m ()
debugM = scriptLog L.debugM
infoM = scriptLog L.infoM
noticeM = scriptLog L.noticeM
warningM = scriptLog L.warningM
errorM = scriptLog L.errorM
criticalM = scriptLog L.criticalM
alertM = scriptLog L.alertM
emergencyM = scriptLog L.emergencyM
