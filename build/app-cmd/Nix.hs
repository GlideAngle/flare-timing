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
        need [ "nix-aeson-via"
             , "nix-siggy-chardust"
             , "nix-tasty-compare"

             , "nix-flight-cmd"
             , "nix-flight-comp"
             , "nix-flight-fsdb"
             , "nix-flight-gap"
             , "nix-flight-igc"
             , "nix-flight-kml"
             , "nix-flight-latlng"
             , "nix-flight-lookup"
             , "nix-flight-mask"
             , "nix-flight-scribe"
             , "nix-flight-task"
             , "nix-flight-track"
             , "nix-flight-units"

             , "nix-flare-timing"
             ]

    phony "nix-aeson-via" $ cmd Shell (nixFor "aeson-via")
    phony "nix-siggy-chardust" $ cmd Shell (nixFor "siggy-chardust")
    phony "nix-tasty-compare" $ cmd Shell (nixFor "tasty-compare")

    phony "nix-flight-cmd" $ cmd Shell (nixFor "flight-cmd")
    phony "nix-flight-comp" $ cmd Shell (nixFor "flight-comp")
    phony "nix-flight-fsdb" $ cmd Shell (nixFor "flight-fsdb")
    phony "nix-flight-gap" $ cmd Shell (nixFor "flight-gap")
    phony "nix-flight-igc" $ cmd Shell (nixFor "flight-igc")
    phony "nix-flight-kml" $ cmd Shell (nixFor "flight-kml")
    phony "nix-flight-latlng" $ cmd Shell (nixFor "flight-latlng")
    phony "nix-flight-lookup" $ cmd Shell (nixFor "flight-lookup")
    phony "nix-flight-mask" $ cmd Shell (nixFor "flight-mask")
    phony "nix-flight-scribe" $ cmd Shell (nixFor "flight-scribe")
    phony "nix-flight-task" $ cmd Shell (nixFor "flight-task")
    phony "nix-flight-track" $ cmd Shell (nixFor "flight-track")
    phony "nix-flight-units" $ cmd Shell (nixFor "flight-units")

    phony "nix-flare-timing" $ cmd Shell (nixFor "flare-timing")
