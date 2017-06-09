{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
import           Data.Aeson
import           ExternalScripts
import           Marvin.Prelude
import           Test.Hspec

-- TODO add tests. I'm not really sure what to test unfortunately.
-- A lot of the code in this repo is just about interacting with external API's


deriving instance Eq ExternalScript
deriving instance Show ExternalScript

testRandom :: Spec
testRandom =
    describe "randomFrom" $
        it "returns the first element in a one element list" $
            randomFrom [4] `shouldReturn` (4 :: Int)


testExternalScriptFormat :: Spec
testExternalScriptFormat =
    describe "fromJSON" $ do
        it "accepts the string form" $
            decode "\"Some.Module\"" `shouldBe` Just (ModuleOnly "Some.Module")
        it "accepts the array form" $
            decode "[\"Some.Module\", \"script\"]" `shouldBe` Just (ModuleAndScripts "Some.Module" ["script"])
        it "accepts the object form 1" $
            decode "{\"module\": \"Some.Module\",\"script\":\"s\"}" `shouldBe` Just (ModuleAndScripts "Some.Module" ["s"])
        it "accepts the object form 2" $
            decode "{\"module\": \"Some.Module\",\"scripts\":[\"s\", \"b\"]}" `shouldBe` Just (ModuleAndScripts "Some.Module" ["s", "b"])


main :: IO ()
main = hspec $ do
    testRandom
    testExternalScriptFormat
