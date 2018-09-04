module Nix (flyPkgs, prefix, buildRules, fromCabalRules, shellRules) where

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
    sequence_ $ buildRule <$> flyPkgs

    phony "nix-build" $
        need ([ "nix-build-detour-via-sci"
             , "nix-build-detour-via-uom"
             , "nix-build-siggy-chardust"
             , "nix-build-tasty-compare"
             , "nix-build-flare-timing"
             , "nix-build-www-flare-timing"
             ]
             ++
             (prefix "nix-flight-" <$> flyPkgs))

    phony "nix-build-detour-via-sci" $ cmd (Cwd "detour-via-sci") Shell "nix build"
    phony "nix-build-detour-via-uom" $ cmd (Cwd "detour-via-uom") Shell "nix build"
    phony "nix-build-siggy-chardust" $ cmd (Cwd "siggy-chardust") Shell "nix build"
    phony "nix-build-tasty-compare" $ cmd (Cwd "tasty-compare") Shell "nix build"
    phony "nix-build-flare-timing" $ cmd (Cwd "flare-timing") Shell "nix build"
    phony "nix-build-www-flare-timing" $ cmd (Cwd "www-flare-timing") Shell "nix build"

    where
        buildRule :: String -> Rules ()
        buildRule s =
            phony ("nix-build-flight-" ++ s) $
                cmd
                    (Cwd s) 
                    Shell "nix build"

fromCabalRules :: Rules ()
fromCabalRules = do
    sequence_ $ fromCabalRule <$> flyPkgs

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

    where
        fromCabalRule :: String -> Rules ()
        fromCabalRule s =
            phony ("cabal2nix-" ++ s) $
                cmd
                    (Cwd s) 
                    Shell
                    (nixFor $ "flight-" ++ s)

shellRules :: Rules ()
shellRules = do
    sequence_ $ shellRule <$> flyPkgs

    phony "nix-shell" $ need
        $ "nix-shell-detour-via-sci"
        : "nix-shell-detour-via-uom"
        : "nix-shell-siggy-chardust"
        : "nix-shell-tasty-compare"
        : "nix-shell-flare-timing"
        : "nix-shell-www-flare-timing"
        : (prefix "nixshell-" <$> flyPkgs)

    phony "nix-shell-detour-via-sci" $
        cmd (Cwd "detour-via-sci") Shell shell

    phony "nix-shell-detour-via-uom" $
        cmd (Cwd "detour-via-uom") Shell shell

    phony "nix-shell-siggy-chardust" $
        cmd (Cwd "siggy-chardust") Shell shell

    phony "nix-shell-tasty-compare" $
        cmd (Cwd "tasty-compare") Shell shell

    phony "nix-shell-flare-timing" $
        cmd (Cwd "flare-timing") Shell shell

    phony "nix-shell-www-flare-timing" $
        cmd (Cwd "www") Shell shell

    where
        shellRule :: String -> Rules ()
        shellRule s =
            phony ("nix-shell-" ++ s) $ cmd (Cwd s) Shell shell
