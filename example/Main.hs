module Main where

import Marvin.Prelude


script1 = defineScript "script-1" $ do
    respond "hello|goodbye" $ do
        m <- getMatch
        send $ (m `indexEx` 1 ) ++ " to you too"


scripts = [script1]


main = runServer scripts
