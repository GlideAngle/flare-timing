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

cleanRules :: Rules ()
cleanRules =
    phony "clean-cmd-apps" $
        removeFilesAfter "__shake-build" [ "//*" ] 

root :: FilePath
root = "flare-timing"

buildRules :: Rules ()
buildRules = do
    phony "cmd-apps" $
        need [ "mask-cmd"
             , "comp-serve"
             , "comp-cmd"
             , "fsdb-cmd"
             , "igc-cmd"
             , "kml-cmd"
             ]

    phony "mask-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:mask-tracks")

    phony "comp-serve" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:comp-serve")

    phony "comp-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:comp-xml-to-yaml")

    phony "fsdb-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:fsdb-parser")

    phony "igc-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:igc-parser")

    phony "kml-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:kml-parser")
