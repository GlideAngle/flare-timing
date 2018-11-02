module Pkg (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Cwd, Shell)
    , (%>)
    , phony
    , cmd
    , need
    )

import Development.Shake.FilePath ((<.>), (</>))

type Folder = String
type Pkg = String

-- | Pairs of folder and package name.
pkgs :: [(Folder, Pkg)]
pkgs =
    [ ("detour-via-sci", "detour-via-sci")
    , ("detour-via-uom", "detour-via-uom")
    , ("build", "build-flare-timing")
    , ("cmd", "flight-cmd")
    , ("comp", "flight-comp")
    , ("earth", "flight-earth")
    , ("flare-timing", "flare-timing")
    , ("fsdb", "flight-fsdb")
    , ("gap", "flight-gap")
    , ("igc", "flight-igc")
    , ("kml", "flight-kml")
    , ("latlng", "flight-latlng")
    , ("lookup", "flight-lookup")
    , ("mask", "flight-mask")
    , ("route", "flight-route")
    , ("scribe", "flight-scribe")
    , ("siggy-chardust", "siggy-chardust")
    , ("span", "flight-span")
    , ("task", "flight-task")
    , ("tasty-compare", "tasty-compare")
    , ("track", "flight-track")
    , ("units", "flight-units")
    , ("www", "www-flare-timing")
    , ("zone", "flight-zone")
    ] 

format :: (Folder, Pkg) -> Rules ()
format (folder, _) =
    phony ("dhall-format-" ++ folder)
    $ cmd Shell ("__shake-build/dhall format --inplace " ++ (folder </> "package.dhall"))

hpack :: (Folder, Pkg) -> Rules ()
hpack (folder, _) =
    phony ("hpack-dhall-" ++ folder) $ do
        need ["dhall-format-" ++ folder]
        cmd (Cwd folder) Shell ("../__shake-build/dhall-hpack-cabal --package-dhall=package.dhall")

cabal :: (Folder, Pkg) -> Rules ()
cabal (folder, pkg) =
    folder </> pkg <.> "cabal" %> \_ -> need ["hpack-dhall-" ++ folder]

buildRules :: Rules ()
buildRules = do
    sequence_ $ format <$> pkgs
    sequence_ $ hpack <$> pkgs
    sequence_ $ cabal <$> pkgs
    phony "dhall-format" $ need $ (\(x, _) -> "dhall-format-" ++ x) <$> pkgs
    phony "hpack-dhall" $ need $ (\(x, _) -> "hpack-dhall-" ++ x) <$> pkgs
    phony "cabal-files" $ need $ (\(x, y) -> x </> y <.> "cabal") <$> pkgs
