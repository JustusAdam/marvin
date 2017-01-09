{-# LANGUAGE NamedFieldPuns #-}
module Marvin.Adapter.Shell where


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Data.Char                       (isSpace)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Lazy                  as L
import           Data.Time.Clock                 (getCurrentTime)
import           Marvin.Adapter
import           Marvin.Internal                 (defaultBotName)
import           Marvin.Internal.Types
import           Marvin.Interpolate.String
import           Marvin.Run                      (lookupFromAppConfig)
import           System.Console.Haskeline


data ShellAdapter = ShellAdapter
    { output :: MVar (Maybe L.Text)
    }


instance IsAdapter ShellAdapter where
    type User ShellAdapter = ()
    type Channel ShellAdapter = ()

    adapterId = "shell"
    messageChannel ShellAdapter{output} _ = putMVar output . Just
    getUsername _ _ = return "shell"
    getChannelName _ _ = return "shell"
    resolveChannel _ _ = return $ Just ()
    runWithAdapter cfg initializer = do
        bot <- liftIO $ fromMaybe defaultBotName <$> lookupFromAppConfig cfg "name"
        out <- liftIO newEmptyMVar
        let ada = ShellAdapter out
        handler <- liftIO $ initializer ada
        liftIO $ runInputT defaultSettings $ forever $ do
            input <- getInputLine $(isS "#{bot}> ")
            case input of
                Nothing -> return ()
                Just i -> do
                    h <- liftIO $ async $ do
                        botname <- L.toLower . fromMaybe defaultBotName <$> liftIO (lookupFromAppConfig cfg "name")
                        let mtext = L.pack i
                        ts <- TimeStamp <$> getCurrentTime
                        handler $ case L.stripPrefix botname $ L.stripStart mtext of
                                    Just cmd | fmap (isSpace . fst) (L.uncons cmd) == Just True ->
                                        CommandEvent () () (L.stripStart cmd) ts
                                    _ -> MessageEvent () () mtext ts
                        putMVar out Nothing
                    whileJust_ (liftIO $ takeMVar out) $ outputStrLn . L.unpack
                    liftIO $ wait h
