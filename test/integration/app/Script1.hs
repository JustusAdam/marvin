{-# LANGUAGE TemplateHaskell #-}
module Script1 (
    script
    ) where


import           Marvin.Prelude


script :: IsAdapter a => ScriptInit a
script = defineScript "test" $ do
    hear (r [CaseInsensitive] "ping") $ do
        msg <- getMessage
        logInfoN $(isT "%{content msg}")
        send "Pong"
    respond "hello" $
        reply "Hello to you too"
    enter $
        send "Hello"
