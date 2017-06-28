{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Lens
import           Control.Monad.Extra
import           Data.Aeson.Lens
import           Data.Maybe
import           Data.Text           (Text)
import           Data.Yaml
import           System.Directory
import           System.Environment
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


-- | In the .travis.yml file I pass the stack yaml as STACK_YAML env variable.
-- This is necessary as the extra-deps are different for some lts versions
-- However the yaml file in STACK_YAML cannot be used by the integration tests,
-- as they require both marvins source directory as well as their own to be
-- recorded in the `packages` key.
-- This function reads the stack.yaml, modifies the `packages` section and
-- writes a new stack.yaml to the target path which the tests pass to stack via
-- then `--stack-yaml` command line parameter
getAndModifyStackYaml :: FilePath -> IO ()
getAndModifyStackYaml targetPath = do
    path <- fromMaybe "stack.yaml" <$> lookupEnv "STACK_YAML"
    Just yamlFile <- decodeFile path
    encodeFile targetPath $ (yamlFile :: Value) & key "packages" .~ packageObject
  where
    packageObject = toJSON [".", ".." :: Text]


getResolver :: IO String
getResolver = fromMaybe "lts" <$> lookupEnv "STACK_RESOLVER"


prepareBuilder :: FilePath -> IO (String -> [String] -> IO (), IO ())
prepareBuilder dir =
    lookupEnv "BUILD" >>= \case
        Just "cabal" ->
            return
                ( callProcess
                , callProcess "cabal" ["build"])
        _ -> do
            resolver <- getResolver
            getAndModifyStackYaml (dir </> "stack.yaml")
            return
                ( \cmd args -> callProcess "stack" $ ["exec", "--resolver", resolver, "--stack-yaml", "stack.yaml", "--", cmd] ++ args
                , callProcess "stack" $ ["build", "--resolver", resolver, "--stack-yaml", "stack.yaml"] ++ stackBuildArgs
                )


-- | This test is used to make sure the initializer produces a compileable project
runInitAndCompileResult :: IO ()
runInitAndCompileResult =
    withTempDirectory "." ".test" $ \dir -> do
        (executor, builder) <- prepareBuilder dir
        withCurrentDirectory dir $ do
            executor "marvin-init" ["-a", "shell", "testbot"]
            builder


compileIntegration :: IO ()
compileIntegration =
    withTempDirectory "." ".test" $ \dir -> do
        copyDirectory "test/integration" dir
        (executor, builder) <- prepareBuilder dir
        getAndModifyStackYaml (dir </> "stack.yaml")
        withCurrentDirectory dir builder


main :: IO ()
main = hspec $ do
    describe "the integration test" $
        it "compiles the integration test suite" $
            compileIntegration `shouldReturn` ()
    describe "initializer" $
        it "produces a compileable project" $
            runInitAndCompileResult `shouldReturn` ()
