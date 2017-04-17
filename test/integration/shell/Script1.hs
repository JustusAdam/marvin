{-# LANGUAGE TemplateHaskell #-}
module Script1 (
    script
    ) where


import qualified Data.Text.Lazy           as L
import qualified Data.Version             as V
import           Marvin.Prelude
import qualified Paths_marvin_integration as P
import System.Directory


script :: (IsAdapter a, HasFiles a) => ScriptInit a
script = defineScript "test" $ do
    hear (r [CaseInsensitive] "^ping$") $ do
        msg <- getMessage
        logInfoN $(isT "#{msg}")
        send "Pong"
    respond "hello" $
        reply "Hello to you too"
    exit $ do
        user <- getUser
        send $(isL "Goodbye #{user^.username}")
    topic $ do
        t <- getTopic
        send $(isL "The new topic is #{t}")
    respond "^where are you\\?$" $ do
        loc <- requireConfigVal "location"
        send $(isL "I am running on #{loc :: L.Text}")

    topicIn "#testing" $ do
        t <- getTopic
        messageChannel "#random" $(isL "The new topic in testing is \"#{t}\"")
    enterIn "#random" $ do
        u <- getUser
        send $(isL "#{u^.username} just entered random")

    respond "^version\\??$" $ send $(isL "marvins integration test, version #{V.showVersion P.version}")

    hear "^bot name\\??$" $ do
        n <- getBotName
        send $(isL "My name is #{n}, nice to meet you.")

    fileShared $ do
        f <- getRemoteFile
        send $(isL "A file of name #{f^.name} and type #{f^.fileType}")
        when (f^.fileType == Nothing) $ do
            content <- readTextFile f
            maybe (return ()) send content

    fileSharedIn "#testing" $ do
        f <- getRemoteFile
        when (f^.fileType == Just "jpg") $ do
            res <- saveFileToDir f "downloaded"
            send $ case res of 
                      Left err -> $(isL "Failed to save file: #{err}")
                      Right path -> $(isL "File saved to path: #{path}")

    respond "^upload (.+)$" $ do
        [_, path] <- getMatch

        e <- liftIO $ doesFileExist (L.unpack path)

        when e $ do
            chan <- getChannel

            f <- newLocalFile path (FileOnDisk path)

            res <- shareFile f [chan]

            case res of 
                Left err -> send $(isL "Failed to share file: #{err}")
                Right _ -> return ()

