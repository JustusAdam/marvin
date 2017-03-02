import Test.Hspec
import System.IO.Temp
import System.Directory
import System.Process

-- | This test is used to make sure the initializer produces a compileable project
runInitAndCompileResult :: IO ()
runInitAndCompileResult = 
    withTempDirectory "." ".test" $ \dir ->
        withCurrentDirectory dir $ do
            callProcess "stack" ["exec", "--", "marvin-init", "-a", "shell", "testbot"]
            copyFile "../test/resources/stack.yaml" "stack.yaml"
            callProcess "stack" ["build"]


main :: IO ()
main = hspec $
    describe "initializer" $
        it "produces a runnable project" $
            runInitAndCompileResult `shouldReturn` ()
