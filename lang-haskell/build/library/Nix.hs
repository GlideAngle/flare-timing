module Nix
    ( flyPkgs
    , prefix
    , buildRules
    , fromCabalRules
    , shellRules
    , cleanRules
    ) where

import Development.Shake
    ( Rules
    , CmdOption(Shell, Cwd, EchoStdout)
    , (%>)
    , phony
    , cmd
    , need
    , removeFilesAfter
    , copyFile'
    , putNormal
    )

import Development.Shake.FilePath ((<.>), (</>), dropFileName)

-- | The names of the hlint tests
flyPkgs :: [String]
flyPkgs =
    [ "clip"
    , "cmd"
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
    , "time"
    , "track"
    , "units"
    , "zone"
    ]

shellPkgs :: [String]
shellPkgs =
    [ "detour-via-sci"
    , "detour-via-uom"
    , "siggy-chardust"
    , "tasty-compare"
    , "flare-timing"
    , "app-serve"
    ]
    ++ flyPkgs

prefix :: String -> String -> String
prefix prefix' s = prefix' ++ s

buildRules :: Rules ()
buildRules = do
    sequence_ $ buildRule <$> flyPkgs

    phony "nix-build" $
        need ([ "nix-build-detour-via-sci"
             , "nix-build-detour-via-uom"
             , "nix-build-siggy-chardust"
             , "nix-build-tasty-compare"
             , "nix-build-flare-timing"
             , "nix-build-app-serve"
             ]
             ++
             (prefix "nix-build-flight-" <$> flyPkgs))

    phony' "detour-via-sci"
    phony' "detour-via-uom"
    phony' "siggy-chardust"
    phony' "tasty-compare"
    phony' "flare-timing"
    phony' "app-serve"

    where
        phony' s = do
            phony (prefix "nix-build-" s) $
                cmd
                    (EchoStdout True)
                    (Cwd s)
                    ["nix-build"]

        buildRule :: String -> Rules ()
        buildRule s =
            phony ("nix-build-flight-" ++ s) $
                cmd
                    (EchoStdout True)
                    (Cwd s)
                    ["nix-build"]

fromCabalRules :: Rules ()
fromCabalRules = do
    sequence_ $ fromCabalRule <$> flyPkgs

    phony "cabal2nix" $ need
        $ "cabal2nix-detour-via-sci"
        : "cabal2nix-detour-via-uom"
        : "cabal2nix-siggy-chardust"
        : "cabal2nix-tasty-compare"
        : "cabal2nix-flare-timing"
        : "cabal2nix-app-serve"
        : (prefix "cabal2nix-" <$> flyPkgs)

    phony "cabal2nix-detour-via-sci" $
        cmd
            (Cwd "detour-via-sci")
            Shell
            (cabal2nix "detour-via-sci")

    phony "cabal2nix-detour-via-uom" $
        cmd
            (Cwd "detour-via-uom")
            Shell
            (cabal2nix "detour-via-uom")

    phony "cabal2nix-siggy-chardust" $
        cmd
            (Cwd "siggy-chardust")
            Shell
            (cabal2nix "siggy-chardust")

    phony "cabal2nix-tasty-compare" $
        cmd
            (Cwd "tasty-compare")
            Shell
            (cabal2nix "tasty-compare")

    phony "cabal2nix-flare-timing" $
        cmd
            (Cwd "flare-timing")
            Shell
            (cabal2nix "flare-timing")

    phony "cabal2nix-app-serve" $
        cmd
            (Cwd "app-serve")
            Shell
            (cabal2nix "app-serve")

    where
        fromCabalRule :: String -> Rules ()
        fromCabalRule s =
            phony ("cabal2nix-" ++ s) $
                cmd
                    (Cwd s)
                    Shell
                    (cabal2nix $ "flight-" ++ s)

        cabal2nix :: String -> String
        cabal2nix x =
            "cabal2nix --no-haddock --no-check . > " ++ (x <.> ".nix")

cleanRules :: Rules ()
cleanRules = do
    phony "clean-nix-shell-files" $
        removeFilesAfter "." ["//shell.nix", "//drv.nix"]

shellRules :: Rules ()
shellRules = do
    phony "nix-shell" $ need (drvs ++ shells)

    "*/shell.nix" %> \out -> do
        need ["nix/hard-shell.nix"]
        putNormal $ "# copyfile (for " ++ out ++ ")"
        copyFile' "nix/hard-shell.nix" out

    "*/drv.nix" %> \out -> do
        let dir = dropFileName out
        need [dir </> "shell.nix"]
        cmd
            (Cwd dir)
            Shell "cabal2nix --shell . > drv.nix"

    where
        drvs = (\s -> s </> "drv.nix") <$> shellPkgs
        shells = (\s -> s </> "shell.nix") <$> shellPkgs
