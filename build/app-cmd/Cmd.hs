module Cmd (buildRules, cleanRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , removeFilesAfter
    , phony
    , cmd
    , need
    )
import Development.Shake.FilePath (FilePath)

cmdFor :: String -> String
cmdFor x =
    "stack build " ++ x ++ " --copy-bins"

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
    [ "comp-xml-to-yaml"
    , "mask-tracks"
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

buildRules :: Rules ()
buildRules = do
    phony "cmd-test-apps" $ need testApps

    phony "cmd-apps" $ need prodApps

    phony "comp-xml-to-yaml" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:comp-xml-to-yaml")

    phony "mask-tracks" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:mask-tracks")

    phony "comp-serve" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:comp-serve")

    phony "test-fsdb-parser" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:test-fsdb-parser")

    phony "test-igc-parser" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:test-igc-parser")

    phony "test-kml-parser" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:test-kml-parser")
