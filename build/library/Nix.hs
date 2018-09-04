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
    , "earth"
    , "fsdb"
    , "gap"
    , "igc"
    , "kml"
    , "latlng"
    , "lookup"
    , "mask"
    , "route"
    , "scribe"
    , "span"
    , "task"
    , "track"
    , "units"
    , "zone"
    ] 

prefix :: String -> String -> String
prefix prefix' s = prefix' ++ s

nixFor :: String -> String
nixFor x =
    "cabal2nix --no-haddock --no-check . > " ++ (x <.> ".nix")

shell :: String
shell =
    "cabal2nix --shell . > shell.nix"

buildRules :: Rules ()
buildRules = do
    phony "nix-build" $
        need [ "nix-detour-via-sci"
             , "nix-detour-via-uom"
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
             , "nix-www-flare-timing"
             ]

    phony "nix-detour-via-sci" $ cmd (Cwd "detour-via-sci") Shell "nix build"
    phony "nix-detour-via-uom" $ cmd (Cwd "detour-via-uom") Shell "nix build"
    phony "nix-siggy-chardust" $ cmd (Cwd "siggy-chardust") Shell "nix build"
    phony "nix-tasty-compare" $ cmd (Cwd "tasty-compare") Shell "nix build"

    phony "nix-flight-cmd" $ cmd (Cwd "cmd") Shell "nix build"
    phony "nix-flight-comp" $ cmd (Cwd "comp") Shell "nix build"
    phony "nix-flight-fsdb" $ cmd (Cwd "fsdb") Shell "nix build"
    phony "nix-flight-gap" $ cmd (Cwd "gap") Shell "nix build"
    phony "nix-flight-igc" $ cmd (Cwd "igc") Shell "nix build"
    phony "nix-flight-kml" $ cmd (Cwd "kml") Shell "nix build"
    phony "nix-flight-latlng" $ cmd (Cwd "latlng") Shell "nix build"
    phony "nix-flight-lookup" $ cmd (Cwd "lookup") Shell "nix build"
    phony "nix-flight-mask" $ cmd (Cwd "mask") Shell "nix build"
    phony "nix-flight-route" $ cmd (Cwd "route") Shell "nix build"
    phony "nix-flight-scribe" $ cmd (Cwd "scribe") Shell "nix build"
    phony "nix-flight-span" $ cmd (Cwd "span") Shell "nix build"
    phony "nix-flight-task" $ cmd (Cwd "task") Shell "nix build"
    phony "nix-flight-track" $ cmd (Cwd "track") Shell "nix build"
    phony "nix-flight-units" $ cmd (Cwd "units") Shell "nix build"
    phony "nix-flight-zone" $ cmd (Cwd "zone") Shell "nix build"

    phony "nix-flare-timing" $ cmd (Cwd "flare-timing") Shell "nix build"
    phony "nix-www-flare-timing" $ cmd (Cwd "www-flare-timing") Shell "nix build"

nixRule :: String -> Rules ()
nixRule s =
    phony ("cabal2nix-" ++ s) $
        cmd
            (Cwd s) 
            Shell
            (nixFor $ "flight-" ++ s)

nixRules :: Rules ()
nixRules = do
    sequence_ $ nixRule <$> flyPkgs

    phony "cabal2nix" $ need
        $ "cabal2nix-detour-via-sci"
        : "cabal2nix-detour-via-uom"
        : "cabal2nix-siggy-chardust"
        : "cabal2nix-tasty-compare"
        : "cabal2nix-flare-timing"
        : "cabal2nix-www-flare-timing"
        : (prefix "cabal2nix-" <$> flyPkgs)

    phony "cabal2nix-detour-via-sci" $
        cmd
            (Cwd "detour-via-sci")
            Shell
            (nixFor "detour-via-sci")

    phony "cabal2nix-detour-via-uom" $
        cmd
            (Cwd "detour-via-uom")
            Shell
            (nixFor "detour-via-uom")

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

    phony "cabal2nix-flare-timing" $
        cmd
            (Cwd "flare-timing")
            Shell
            (nixFor "flare-timing")

    phony "cabal2nix-www-flare-timing" $
        cmd
            (Cwd "www")
            Shell
            (nixFor "www-flare-timing")

shellRule :: String -> Rules ()
shellRule s =
    phony ("nixshell-" ++ s) $ cmd (Cwd s) Shell shell

shellRules :: Rules ()
shellRules = do
    sequence_ $ shellRule <$> flyPkgs

    phony "nixshell" $ need
        $ "nixshell-detour-via-sci"
        : "nixshell-detour-via-uom"
        : "nixshell-siggy-chardust"
        : "nixshell-tasty-compare"
        : "nixshell-flare-timing"
        : "nixshell-www-flare-timing"
        : (prefix "nixshell-" <$> flyPkgs)

    phony "nixshell-detour-via-sci" $
        cmd (Cwd "detour-via-sci") Shell shell

    phony "nixshell-detour-via-uom" $
        cmd (Cwd "detour-via-uom") Shell shell

    phony "nixshell-siggy-chardust" $
        cmd (Cwd "siggy-chardust") Shell shell

    phony "nixshell-tasty-compare" $
        cmd (Cwd "tasty-compare") Shell shell

    phony "nixshell-flare-timing" $
        cmd (Cwd "flare-timing") Shell shell

    phony "nixshell-www-flare-timing" $
        cmd (Cwd "www") Shell shell
