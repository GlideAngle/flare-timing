module Nix (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell)
    , phony
    , cmd
    , need
    )

nixFor :: String -> String
nixFor x =
    "nix-build \"<nixpkgs>\" -A haskellPackages." ++ x

buildRules :: Rules ()
buildRules = do
    phony "nix" $
        need [ "nix-siggy-chardust"
             , "nix-flight-comp"
             , "nix-flight-fsdb"
             , "nix-flight-gap"
             , "nix-flight-igc"
             , "nix-flight-kml"
             , "nix-flight-track"
             , "nix-flight-task"
             , "nix-flare-timing"
             ]

    phony "nix-siggy-chardust" $ cmd Shell (nixFor "siggy-chardust")
    phony "nix-flight-comp" $ cmd Shell (nixFor "flight-comp")
    phony "nix-flight-fsdb" $ cmd Shell (nixFor "flight-fsdb")
    phony "nix-flight-gap" $ cmd Shell (nixFor "flight-gap")
    phony "nix-flight-igc" $ cmd Shell (nixFor "flight-igc")
    phony "nix-flight-kml" $ cmd Shell (nixFor "flight-kml")
    phony "nix-flight-track" $ cmd Shell (nixFor "flight-track")
    phony "nix-flight-task" $ cmd Shell (nixFor "flight-task")
    phony "nix-flare-timing" $ cmd Shell (nixFor "flare-timing")
