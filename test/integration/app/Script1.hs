module Script1 (
    script
    ) where


import           Marvin.Prelude


script :: IsAdapter a => ScriptInit a
script = defineScript "test" $ do
    hear (r [CaseInsensitive] "ping") $ do
        msg <- getMessage
        infoM (content msg)
        send "Pong"
    respond "hello" $
        reply "Hello to you too"
