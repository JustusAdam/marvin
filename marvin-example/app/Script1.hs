module Script1 where
    
import Marvin.Prelude


script1 = defineScript "script-1" $ do
    respond "hello|goodbye" $ do
        m <- getMatch
        send $ (m `indexEx` 0) ++ " to you too"

