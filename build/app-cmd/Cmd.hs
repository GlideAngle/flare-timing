module Cmd (buildRules, cleanRules, testRules, lintRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Development.Shake.FilePath (FilePath)

cmdTestFor :: String -> String
cmdTestFor x =
    "stack test " ++ x

cmdBuildFor :: String -> String
cmdBuildFor x =
    "stack build " ++ x ++ " --copy-bins"

-- | The names of the hlint tests
lintPkgs :: [String]
lintPkgs =
    [ "units"
    , "zone"
    , "track"
    , "task"
    , "mask"
    , "latlng"
    , "fsdb"
    , "igc"
    , "kml"
    , "comp"
    ] 

type Pkg = String
type Test = String

-- | The pairs are names of the pkg and test.
testPkgs :: [(Pkg, Test)]
testPkgs =
    [ ("task", "task")
    , ("kml", "parse")
    , ("fsdb", "parse")
    , ("gap", "score")
    ] 

-- | The names of the test app executables
testApps :: [String]
testApps =
    [ "test-fsdb-parser"
    , "test-igc-parser"
    , "test-kml-parser"
    ] 

-- | The names of the production app executables
prodApps :: [String]
prodApps =
    [ "extract-task"
    , "task-length"
    , "cross-zone"
    , "align-time"
    , "tag-zone"
    , "mask-track"
    , "comp-serve"
    ] 

cleanRules :: Rules ()
cleanRules = do
    phony "clean-prod-apps" $
        removeFilesAfter "__shake-build" prodApps

    phony "clean-test-apps" $
        removeFilesAfter "__shake-build" testApps

root :: FilePath
root = "flare-timing"

lintRule :: String -> Rules ()
lintRule s =
    phony ("lint-" ++ s) $
        cmd
            (Cwd s) 
            Shell
            (cmdTestFor "flight-" ++ s ++ ":hlint")

lintRules :: Rules ()
lintRules = do
    _ <- sequence $ lintRule <$> lintPkgs

    phony "lint" $ need
        $ "lint-build"
        : "lint-flare-timing"
        : ((\x -> "lint-" ++ x) <$> lintPkgs)

    phony "lint-build" $
        cmd
            (Cwd "build")
            Shell
            (cmdTestFor "build-flare-timing:hlint")

    phony "lint-flare-timing" $
        cmd
            (Cwd "flare-timing")
            Shell
            (cmdTestFor "flare-timing:hlint")

testRule :: (Pkg, Test) -> Rules ()
testRule (pkg, test) =
    phony ("test-" ++ pkg) $
        cmd
            (Cwd pkg)
            Shell
            (cmdTestFor "flight-" ++ pkg ++":" ++ test)

testRules :: Rules ()
testRules = do
    _ <- sequence $ testRule <$> testPkgs
    phony "test" $ need $ (\(x, _) -> "test-" ++ x) <$> testPkgs

buildRules :: Rules ()
buildRules = do
    phony "test-apps" $ need testApps

    phony "prod-apps" $ need prodApps

    phony "extract-task" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:extract-task")

    phony "task-length" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:task-length")

    phony "cross-zone" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:cross-zone")

    phony "tag-zone" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:tag-zone")

    phony "align-time" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:align-time")

    phony "mask-track" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:mask-track")

    phony "comp-serve" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:comp-serve")

    phony "test-fsdb-parser" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:test-fsdb-parser")

    phony "test-igc-parser" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:test-igc-parser")

    phony "test-kml-parser" $
        cmd
            (Cwd root)
            Shell
            (cmdBuildFor "flare-timing:test-kml-parser")
