{-# LANGUAGE NamedFieldPuns #-}
module Marvin.Adapter.Shell where


import Marvin.Adapter
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import System.Console.Haskeline
import Control.Monad
import Control.Concurrent.Async.Lifted
import Control.Monad.Loops
import Control.Monad.IO.Class
import Marvin.Internal.Types


data ShellAdapter = ShellAdapter
    { outputChan :: TChan L.Text
    }


instance IsAdapter ShellAdapter where
    type User ShellAdapter = ()
    type Channel ShellAdapter = ()

    adapterId = "shell"
    messageChannel ShellAdapter{outputChan} _ = liftIO . atomically . writeTChan outputChan
    getUsername _ _ = return "shell"
    getChannelName _ _ = return "shell"
    resolveChannel _ _ = return $ Just ()
    runWithAdapter cfg initializer = do
        outChan <- liftIO $ atomically newTChan
        let ada = ShellAdapter outChan
        handler <- liftIO $ initializer ada
        liftIO $ runInputT defaultSettings $ forever $ do
            input <- getInputLine "> "
            case input of
                Nothing -> return ()
                Just i -> do
                    liftIO $ handler (MessageEvent (Message () () (L.pack i) (TimeStamp 0)))
                    whileM_ (fmap not $ liftIO $ atomically $ isEmptyTChan outChan) $ do
                        msg <- liftIO $ atomically $ readTChan outChan
                        outputStrLn (L.unpack msg)
