import           Control.Monad.Extra
import           System.Directory
import           System.FilePath
import           System.Info.Extra
import           System.IO.Temp
import           System.Process
import           Test.Hspec

stackBuildArgs :: [String]
stackBuildArgs
    | isMac = ["--extra-lib-dirs=/usr/local/opt/icu4c/lib", "--extra-include-dirs=/usr/local/opt/icu4c/include"]
    | otherwise = []

copyDirectory :: FilePath -> FilePath -> IO ()
copyDirectory source target = do
    createDirectoryIfMissing True target
    (dirs, files) <- getDirectoryContents source >>=
        partitionM (doesDirectoryExist . (source </>))
        . filter (\a -> not $ a == "." || a == "..")
    mapM_ (copyFile <$> (source </>) <*> (target </>)) files
    mapM_ (copyDirectory <$> (source </>) <*> (target </>)) dirs

-- | This test is used to make sure the initializer produces a compileable project
runInitAndCompileResult :: IO ()
runInitAndCompileResult =
    withTempDirectory "." ".test" $ \dir ->
        withCurrentDirectory dir $ do
            callProcess "stack" ["exec", "--", "marvin-init", "-a", "shell", "testbot"]
            copyFile "../test/resources/stack.yaml" "stack.yaml"
            callProcess "stack" $ "build" : stackBuildArgs


compileIntegration :: IO ()
compileIntegration =
    withTempDirectory "." ".test" $ \dir -> do
        copyDirectory "test/integration" dir
        copyFile "test/resources/stack.yaml" (dir </> "stack.yaml")
        withCurrentDirectory dir $ callProcess "stack" $ "build" : stackBuildArgs


main :: IO ()
main = hspec $ do
    describe "the integration test" $
        it "compiles the integration test suite" $
            compileIntegration `shouldReturn` ()
    describe "initializer" $
        it "produces a compileable project" $
            runInitAndCompileResult `shouldReturn` ()
