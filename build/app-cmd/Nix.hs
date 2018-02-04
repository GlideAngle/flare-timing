module Nix (flyPkgs, prefix, buildRules, nixRules, shellRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd)
    , phony
    , cmd
    , need
    )

import Development.Shake.FilePath ((<.>))

-- | The names of the hlint tests
flyPkgs :: [String]
flyPkgs =
    [ "cmd"
    , "comp"
    , "fsdb"
    , "gap"
    , "igc"
    , "kml"
    , "latlng"
    , "lookup"
    , "mask"
    , "scribe"
    , "sphere"
    , "task"
    , "track"
    , "units"
    , "zone"
    , "route"
    , "span"
    ] 

prefix :: String -> String -> String
prefix prefix' s = prefix' ++ s

nixFor :: String -> String
nixFor x =
    "cabal2nix . > " ++ (x <.> ".nix")

shell :: String
shell =
    "cabal2nix --shell . > shell.nix"

buildFor :: String -> String
buildFor x =
    "nix-build \"<nixpkgs>\" -A haskellPackages." ++ x

buildRules :: Rules ()
buildRules = do
    phony "nix" $
        need [ "nix-aeson-via-sci"
             , "nix-aeson-via-uom"
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
             , "nix-flight-zone"
             , "nix-flight-span"

             , "nix-flare-timing"
             ]

    phony "nix-aeson-via-sci" $ cmd Shell (buildFor "aeson-via-sci")
    phony "nix-aeson-via-uom" $ cmd Shell (buildFor "aeson-via-uom")
    phony "nix-siggy-chardust" $ cmd Shell (buildFor "siggy-chardust")
    phony "nix-tasty-compare" $ cmd Shell (buildFor "tasty-compare")

    phony "nix-flight-cmd" $ cmd Shell (buildFor "flight-cmd")
    phony "nix-flight-comp" $ cmd Shell (buildFor "flight-comp")
    phony "nix-flight-fsdb" $ cmd Shell (buildFor "flight-fsdb")
    phony "nix-flight-gap" $ cmd Shell (buildFor "flight-gap")
    phony "nix-flight-igc" $ cmd Shell (buildFor "flight-igc")
    phony "nix-flight-kml" $ cmd Shell (buildFor "flight-kml")
    phony "nix-flight-latlng" $ cmd Shell (buildFor "flight-latlng")
    phony "nix-flight-lookup" $ cmd Shell (buildFor "flight-lookup")
    phony "nix-flight-mask" $ cmd Shell (buildFor "flight-mask")
    phony "nix-flight-scribe" $ cmd Shell (buildFor "flight-scribe")
    phony "nix-flight-task" $ cmd Shell (buildFor "flight-task")
    phony "nix-flight-track" $ cmd Shell (buildFor "flight-track")
    phony "nix-flight-units" $ cmd Shell (buildFor "flight-units")
    phony "nix-flight-zone" $ cmd Shell (buildFor "flight-zone")
    phony "nix-flight-route" $ cmd Shell (buildFor "flight-route")
    phony "nix-flight-span" $ cmd Shell (buildFor "flight-span")

    phony "nix-flare-timing" $ cmd Shell (buildFor "flare-timing")

nixRule :: String -> Rules ()
nixRule s =
    phony ("cabal2nix-" ++ s) $
        cmd
            (Cwd s) 
            Shell
            (nixFor $ "flight-" ++ s)

nixRules :: Rules ()
nixRules = do
    _ <- sequence_ $ nixRule <$> flyPkgs

    phony "cabal2nix" $ need
        $ "cabal2nix-aeson-via-sci"
        : "cabal2nix-aeson-via-uom"
        : "cabal2nix-siggy-chardust"
        : "cabal2nix-tasty-compare"
        : (prefix "cabal2nix-" <$> flyPkgs)

    phony "cabal2nix-aeson-via-sci" $
        cmd
            (Cwd "aeson-via-sci")
            Shell
            (nixFor "aeson-via-sci")

    phony "cabal2nix-aeson-via-uom" $
        cmd
            (Cwd "aeson-via-uom")
            Shell
            (nixFor "aeson-via-uom")

    phony "cabal2nix-siggy-chardust" $
        cmd
            (Cwd "siggy-chardust")
            Shell
            (nixFor "siggy-chardust")

    phony "cabal2nix-tasty-compare" $
        cmd
            (Cwd "tasty-compare")
            Shell
            (nixFor "tasty-compare")

shellRule :: String -> Rules ()
shellRule s =
    phony ("nixshell-" ++ s) $ cmd (Cwd s) Shell shell

shellRules :: Rules ()
shellRules = do
    _ <- sequence_ $ shellRule <$> flyPkgs

    phony "nixshell" $ need
        $ "nixshell-aeson-via-sci"
        : "nixshell-aeson-via-uom"
        : "nixshell-siggy-chardust"
        : "nixshell-tasty-compare"
        : (prefix "nixshell-" <$> flyPkgs)

    phony "nixshell-aeson-via-sci" $
        cmd (Cwd "aeson-via-sci") Shell shell

    phony "nixshell-aeson-via-uom" $
        cmd (Cwd "aeson-via-uom") Shell shell

    phony "nixshell-siggy-chardust" $
        cmd (Cwd "siggy-chardust") Shell shell

    phony "nixshell-tasty-compare" $
        cmd (Cwd "tasty-compare") Shell shell
