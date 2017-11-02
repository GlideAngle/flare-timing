module Cmd (buildRules, cleanRules, testRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Development.Shake.FilePath (FilePath)

cmdBuildFor :: String -> String
cmdBuildFor x =
    "stack build " ++ x ++ " --copy-bins"

-- | The names of the pkgs with tests
testPkgs :: [String]
testPkgs =
    [ "test-units"
    , "test-zone"
    , "test-track"
    , "test-task"
    , "test-mask"
    , "test-comp"
    , "test-flare-timing"
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
    phony "clean-cmd-apps" $
        removeFilesAfter "__shake-build" prodApps

    phony "clean-cmd-test-apps" $
        removeFilesAfter "__shake-build" testApps

root :: FilePath
root = "flare-timing"

testRules :: Rules ()
testRules = do
    phony "test-pkgs" $ need testPkgs

    phony "test-units" $ cmd (Cwd "units") Shell "stack test"
    phony "test-zone" $ cmd (Cwd "zone") Shell "stack test"
    phony "test-track" $ cmd (Cwd "track") Shell "stack test"
    phony "test-task" $ cmd (Cwd "task") Shell "stack test"
    phony "test-mask" $ cmd (Cwd "mask") Shell "stack test"
    phony "test-comp" $ cmd (Cwd "comp") Shell "stack test"
    phony "test-flare-timing" $ cmd (Cwd "flare-timing") Shell "stack test"

buildRules :: Rules ()
buildRules = do
    phony "cmd-test-apps" $ need testApps

    phony "cmd-apps" $ need prodApps

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
