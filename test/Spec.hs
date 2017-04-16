
import           Marvin.Prelude
import           Test.Hspec

-- TODO add tests. I'm not really sure what to test unfortunately.
-- A lot of the code in this repo is just about interacting with external API's

testRandom :: Spec
testRandom =
    describe "randomFrom" $
        it "returns the first element in a one element list" $
            randomFrom [4] `shouldReturn` (4 :: Int)


main :: IO ()
main = hspec testRandom
