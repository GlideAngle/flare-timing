{-# LANGUAGE ScopedTypeVariables #-}

module Pkg (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Cwd, Shell)
    , (%>)
    , phony
    , cmd, cmd_
    , need
    )

import Development.Shake.FilePath ((<.>), (</>))

type Folder = String
type Pkg = String
type CmdApp = String
type Title = String

-- | The names of the folders for dhall-format and hpack-dhall.
dhallPkgs :: [Folder]
dhallPkgs = fst <$> dhallCabal

-- | Pairs of package folder name used by dhall and the produced cabal file
-- name.
dhallCabal :: [(Folder, Pkg)]
dhallCabal =
    [ ("lang-haskell/detour-via-sci", "detour-via-sci")
    , ("lang-haskell/detour-via-uom", "detour-via-uom")
    , ("lang-haskell/build", "ft-build")
    , ("lang-haskell/cmd", "flight-cmd")
    , ("lang-haskell/clip", "flight-clip")
    , ("lang-haskell/comp", "flight-comp")
    , ("lang-haskell/earth", "flight-earth")
    , ("lang-haskell/flare-timing", "flare-timing")
    , ("lang-haskell/fsdb", "flight-fsdb")
    , ("lang-haskell/gap-allot", "flight-gap-allot")
    , ("lang-haskell/gap-effort", "flight-gap-effort")
    , ("lang-haskell/gap-lead", "flight-gap-lead")
    , ("lang-haskell/gap-math", "flight-gap-math")
    , ("lang-haskell/gap-stop", "flight-gap-stop")
    , ("lang-haskell/gap-valid", "flight-gap-valid")
    , ("lang-haskell/gap-weight", "flight-gap-weight")
    , ("lang-haskell/gap", "flight-gap")
    , ("lang-haskell/igc", "flight-igc")
    , ("lang-haskell/kml", "flight-kml")
    , ("lang-haskell/latlng", "flight-latlng")
    , ("lang-haskell/lookup", "flight-lookup")
    , ("lang-haskell/mask", "flight-mask")
    , ("lang-haskell/route", "flight-route")
    , ("lang-haskell/scribe", "flight-scribe")
    , ("lang-haskell/siggy-chardust", "siggy-chardust")
    , ("lang-haskell/span", "flight-span")
    , ("lang-haskell/task", "flight-task")
    , ("lang-haskell/time", "flight-time")
    , ("lang-haskell/track", "flight-track")
    , ("lang-haskell/units", "flight-units")
    , ("lang-haskell/zone", "flight-zone")
    , ("lang-haskell/app-serve", "app-serve")
    , ("lang-haskell/app-view", "app-view")
    , ("lang-haskell/tasty-compare", "tasty-compare")
    ]

readmes :: [(Folder, CmdApp, Title)]
readmes =
    [ ("extract-input", "ft-extract-input", "Extracting Scoring Inputs from FSDB")
    , ("fs-arrival", "fs-arrival", "FS Arrivals")
    , ("fs-effort", "fs-effort", "FS Effort")
    , ("fs-filter", "fs-filter", "Filter the FSDB")
    , ("fs-route", "fs-route", "FS Routes")
    , ("fs-score", "fs-score", "FS Scores")
    , ("task-length", "ft-task-length", "Task Lengths")
    , ("cross-zone", "ft-cross-zone", "Zone Crossings")
    , ("tag-zone", "ft-tag-zone", "Zone Taggings")
    , ("unpack-track", "ft-unpack-track", "Unpacking Tracks for Mapping")
    , ("peg-frame", "ft-peg-frame", "Pegging the Scoring Time Frame")
    , ("align-time", "ft-align-time", "Align Times")
    , ("discard-further", "ft-discard-further", "Discard Further Fixes")
    , ("area-step", "ft-area-step", "Area Steps")
    , ("mask-arrival", "ft-mask-arrival", "Masking Tracks for Arrivar")
    , ("mask-track", "ft-mask-track", "Masking Tracks")
    , ("land-out", "ft-land-out", "Landing Out")
    , ("gap-point", "ft-gap-point", "Tallying the GAP Points")
    ]

dhallRootImports :: [String]
dhallRootImports =
    [ "defaults"
    -- NOTE: Don't include default-tests.dhall as it wipes the embedded comments.
    -- , "default-tests"
    , "default-extensions"
    , "default-extensions-ghcjs"
    , "hlint"
    ]

formatPkg :: Folder -> Rules ()
formatPkg folder =
    phony ("dhall-format-" ++ folder)
    $ cmd Shell ("dhall format --inplace " ++ (folder </> "package.dhall"))

formatRoot :: String -> Rules ()
formatRoot x =
    phony ("dhall-format-" ++ x)
    $ cmd Shell ("dhall format --inplace " ++ (x <.> ".dhall"))

hpack :: Folder -> Rules ()
hpack folder =
    phony ("hpack-dhall-" ++ folder) $ do
        need ["dhall-format-" ++ folder]
        cmd (Cwd folder) Shell ("dhall-hpack-cabal --package-dhall=package.dhall")

cabal :: (Folder, Pkg) -> Rules ()
cabal (folder, pkg) =
    folder </> pkg <.> "cabal" %> \_ -> need ["hpack-dhall-" ++ folder]

readme :: (Folder, CmdApp, Title) -> Rules ()
readme (folder, cmdApp, title) =
    phony ("readme-" ++ cmdApp) $ do
        let path :: String = "lang-haskell/flare-timing/prod-apps" </> folder
        cmd_ (Cwd path) Shell ("echo \"### " ++ title ++ "\" > README.md")
        cmd_ (Cwd path) Shell ("echo >> README.md")
        cmd_ (Cwd path) Shell ("echo \'```\' >> README.md")
        cmd_ (Cwd path) Shell ("echo \"> " ++ cmdApp ++ " --help\" >> README.md")
        cmd_ (Cwd path) Shell (cmdApp ++ " --help >> README.md")
        cmd_ (Cwd path) Shell ("echo \'```\' >> README.md")

buildRules :: Rules ()
buildRules = do
    sequence_ $ formatRoot <$> dhallRootImports
    sequence_ $ formatPkg <$> dhallPkgs
    sequence_ $ hpack <$> dhallPkgs
    sequence_ $ cabal <$> dhallCabal
    sequence_ $ readme <$> readmes
    phony "dhall-format" $ need $ (\x -> "dhall-format-" ++ x) <$> dhallPkgs ++ dhallRootImports
    phony "hpack-dhall" $ need $ (\x -> "hpack-dhall-" ++ x) <$> dhallPkgs
    phony "cabal-files" $ need $ (\(x, y) -> x </> y <.> "cabal") <$> dhallCabal
    phony "readmes" $ need $ (\(_, y, _) -> "readme-" ++ y) <$> readmes
