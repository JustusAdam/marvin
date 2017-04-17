{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module Script1 (
    script
    ) where


import qualified Data.Text.Lazy           as L
import qualified Data.Version             as V
import           Marvin.Prelude
import qualified Paths_marvin_integration as P
import System.Directory
import System.FilePath
import Marvin.Adapter.Slack.RTM

-- Add a test that the HasFiles functionality is available as is to be expected

script :: ScriptInit (SlackAdapter RTM)
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

    fileSharedIn "#testing" $ do
        f <- getRemoteFile
        when (f^.fileType == Just "jpg") $ do
            res <- saveFileToDir f "downloaded"
            send $ case res of 
                      Left err -> $(isL "Failed to save file: #{err}")
                      Right path -> $(isL "File saved to path: #{path}")

    respond "^upload (.+)$" $ do
        [_, path'] <- getMatch

        let path = L.unpack path'

        if  | isAbsolute path -> send "Please provide a relative path"
            | ".." `elem` splitDirectories path -> send "'..' is not allowed in the upload path"
            | otherwise -> do
                e <- liftIO $ doesFileExist path

                if e 
                    then do
                        chan <- getChannel

                        f <- newLocalFile path' (FileOnDisk path')

                        res <- shareFile f [chan]

                        case res of 
                            Left err -> send $(isL "Failed to share file: #{err}")
                            Right _ -> send "File successfully uploaded"
                    else send $(isL "Sorry, but there is no file with the path #{path} here.")
        
