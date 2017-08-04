{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}
module Script1 (
    script
    ) where


import qualified Data.Text.Lazy           as L
import qualified Data.Version             as V
import           Marvin.Adapter.Slack.RTM
import           Marvin.Handler
import           Marvin.Prelude
import qualified Paths_marvin_integration as P
import           System.Directory
import           System.FilePath

-- Add a test that the SupportsFiles functionality is available as is to be expected

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

    topicIn "testing" $ do
        t <- getTopic
        messageChannel "#random" $(isL "The new topic in testing is \"#{t}\"")
    enterIn "random" $ do
        u <- getUser
        send $(isL "#{u^.username} just entered random")

    respond "^version\\??$" echoVersion

    hear "^bot name\\??$" $ do
        n <- getBotName
        send $(isL "My name is #{n}, nice to meet you.")

    fileSharedIn "testing" $ downloadFile True "downloaded"

    respond "^upload (.+)$" $ uploadFile True 1
