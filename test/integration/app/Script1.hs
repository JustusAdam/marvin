module Script1 (
    script
    ) where


import           Marvin.Prelude


script :: IsAdapter a => ScriptInit a
script = defineScript "test" $
    hear ".*" $ do
        msg <- getMessage
        infoM (content msg)
        reply $ "I heard \"" ++ content msg ++ "\""
