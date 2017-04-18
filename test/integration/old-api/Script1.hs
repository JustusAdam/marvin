{-# LANGUAGE TemplateHaskell #-}
module Script1 (
    script
    ) where


import qualified Data.Text.Lazy           as L
import qualified Data.Version             as V
import           Marvin.Handler
import           Marvin.Prelude
import qualified Paths_marvin_integration as P


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
        username <- getUsername user
        send $(isL "Goodbye #{username}")
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
        username <- getUsername user
        send $(isL "#{username} just entered random")

    respond "^version\\??$" echoVersion

    hear "^bot name\\??$" $ do
        n <- getBotName
        send $(isL "My name is #{n}, nice to meet you.")

    fileShared $ do
        f <- getRemoteFile
        content <- readTextFile f
        send $(isL "A file of name #{f^.name}")
        maybe (return ()) send content
