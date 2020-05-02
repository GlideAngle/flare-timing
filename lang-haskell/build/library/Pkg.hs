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

-- | The names of the folders for dhall-format and hpack-dhall.
dhallPkgs :: [Folder]
dhallPkgs = fst <$> dhallCabal

-- | Pairs of package folder name used by dhall and the produced cabal file
-- name.
dhallCabal :: [(Folder, Pkg)]
dhallCabal =

    [ ("detour-via-sci", "detour-via-sci")
    , ("detour-via-uom", "detour-via-uom")
    , ("build", "build-flare-timing")
    , ("cmd", "flight-cmd")
    , ("clip", "flight-clip")
    , ("comp", "flight-comp")
    , ("earth", "flight-earth")
    , ("flare-timing", "flare-timing")
    , ("fsdb", "flight-fsdb")
    , ("gap-base", "flight-gap-base")
    , ("gap-effort", "flight-gap-effort")
    , ("gap-lead", "flight-gap-lead")
    , ("gap-math", "flight-gap-math")
    , ("gap-stop", "flight-gap-stop")
    , ("gap-valid", "flight-gap-valid")
    , ("gap-weight", "flight-gap-weight")
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
    , ("time", "flight-time")
    , ("track", "flight-track")
    , ("units", "flight-units")
    , ("zone", "flight-zone")
    , ("app-serve", "app-serve")
    , ("app-view", "app-view")
    , ("tasty-compare", "tasty-compare")
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

buildRules :: Rules ()
buildRules = do
    sequence_ $ formatRoot <$> dhallRootImports
    sequence_ $ formatPkg <$> dhallPkgs
    sequence_ $ hpack <$> dhallPkgs
    sequence_ $ cabal <$> dhallCabal
    phony "dhall-format" $ need $ (\x -> "dhall-format-" ++ x) <$> dhallPkgs ++ dhallRootImports
    phony "hpack-dhall" $ need $ (\x -> "hpack-dhall-" ++ x) <$> dhallPkgs
    phony "cabal-files" $ need $ (\(x, y) -> x </> y <.> "cabal") <$> dhallCabal
