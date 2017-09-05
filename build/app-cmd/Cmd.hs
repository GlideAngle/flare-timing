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
             , "app-serve-cmd"
             , "comp-xml-to-yaml-cmd"
             , "fsdb-cmd"
             , "igc-cmd"
             , "kml-cmd"
             ]

    phony "mask-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:flight-mask-yaml")

    phony "app-serve-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:flight-yaml-serve")

    phony "comp-xml-to-yaml-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:flight-comp-xml-to-yaml")

    phony "fsdb-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:flight-fsdb-cmd")

    phony "igc-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:flight-igc-cmd")

    phony "kml-cmd" $
        cmd
            (Cwd root)
            Shell
            (cmdFor "flare-timing:flight-kml-cmd")
