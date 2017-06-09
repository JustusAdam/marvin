{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
import           Data.Aeson
import           ExternalScripts
import           Marvin.Prelude
import           Test.Hspec
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Foldable (fold)

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
        let ex1 = ("\"Some.Module\"", ModuleOnly "Some.Module")
            ex2 = ("[\"Some.Module\", \"script\"]", ModuleAndScripts "Some.Module" ["script"])
            ex3 = ("{\"module\": \"Some.Module\",\"script\":\"s\"}", ModuleAndScripts "Some.Module" ["s"])
            ex4 = ("{\"module\": \"Some.Module\",\"scripts\":[\"s\", \"b\"]}", ModuleAndScripts "Some.Module" ["s", "b"])
            test (a, b) = decode a `shouldBe` Just b
        it "accepts the string form" $ test ex1
        it "accepts the array form" $ test ex2
        it "accepts the object form 1" $ test ex3
        it "accepts the object form 2" $ test ex4
        let (jsonIn, valOut) = unzip [ex1, ex2, ex3, ex4]

        it "accepts a combined form as list" $ test ("[" <> fold (intersperse "," jsonIn) <> "]", valOut)


main :: IO ()
main = hspec $ do
    testRandom
    testExternalScriptFormat
