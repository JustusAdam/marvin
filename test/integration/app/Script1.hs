{-# LANGUAGE TemplateHaskell #-}
module Script1 (
    script
    ) where


import qualified Data.Text.Lazy as L
import           Marvin.Prelude


script :: IsAdapter a => ScriptInit a
script = defineScript "test" $ do
    hear (r [CaseInsensitive] "^ping$") $ do
        msg <- getMessage
        logInfoN $(isT "#{content msg}")
        send "Pong"
    respond "hello" $
        reply "Hello to you too"
    exit $ do
        name <- getUser >>= getUsername
        send $(isL "Goodbye #{name}")
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
        u <- getUser >>= getUsername
        send $(isL "#{u} just entered random")
