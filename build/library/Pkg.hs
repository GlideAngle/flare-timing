module Pkg (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell)
    , (%>)
    , phony
    , cmd
    , need
    )

import Development.Shake.FilePath ((<.>), (</>))

-- | The names of the packages for dhall-format and hpack-dhall.
dhallPkgs :: [String]
dhallPkgs = fst <$> dhallCabal

-- | Pairs of package folder name used by dhall and the produced cabal file
-- name.
dhallCabal :: [(String, String)]
dhallCabal =
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
    , ("zone", "flight-zone")
    , ("app-serve", "app-serve")
    , ("app-view", "app-view")
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

formatPkg :: String -> Rules ()
formatPkg x =
    phony ("dhall-format-" ++ x)
    $ cmd Shell ("__shake-build/dhall format --inplace " ++ (x </> "package.dhall"))

formatRoot :: String -> Rules ()
formatRoot x =
    phony ("dhall-format-" ++ x)
    $ cmd Shell ("__shake-build/dhall format --inplace " ++ (x <.> ".dhall"))

hpack :: String -> Rules ()
hpack x =
    phony ("hpack-dhall-" ++ x) $ do
        need ["dhall-format-" ++ x]
        cmd Shell ("__shake-build/hpack-dhall " ++ x)

cabal :: (String, String) -> Rules ()
cabal (x, y) =
    x </> y <.> "cabal" %> \_ -> need ["hpack-dhall-" ++ x]

buildRules :: Rules ()
buildRules = do
    sequence_ $ formatRoot <$> dhallRootImports
    sequence_ $ formatPkg <$> dhallPkgs
    sequence_ $ hpack <$> dhallPkgs
    sequence_ $ cabal <$> dhallCabal
    phony "dhall-format" $ need $ (\x -> "dhall-format-" ++ x) <$> dhallPkgs ++ dhallRootImports
    phony "hpack-dhall" $ need $ (\x -> "hpack-dhall-" ++ x) <$> dhallPkgs
    phony "cabal-files" $ need $ (\(x, y) -> x </> y <.> "cabal") <$> dhallCabal
