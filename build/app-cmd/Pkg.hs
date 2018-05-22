module Pkg (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Shell)
    , phony
    , cmd
    , need
    )

import Development.Shake.FilePath ((</>))

-- | The names of the packages for dhall-format and hpack-dhall.
dhallPkgs :: [String]
dhallPkgs =
    [ "aeson-via-sci"
    , "aeson-via-uom"
    , "build"
    , "cmd"
    , "comp"
    , "earth"
    , "flare-timing"
    , "fsdb"
    , "gap"
    , "igc"
    , "kml"
    , "latlng"
    , "lookup"
    , "mask"
    , "route"
    , "scribe"
    , "siggy-chardust"
    , "span"
    , "task"
    , "track"
    , "units"
    , "www"
    , "zone"
    ] 

hpack :: String -> Rules ()
hpack x =
    phony ("hpack-dhall-" ++ x) $ cmd Shell ("hpack-dhall " ++ x)

format :: String -> Rules ()
format x =
    phony ("dhall-format-" ++ x)
    $ cmd Shell ("dhall-format --inplace " ++ (x </> "package.dhall"))

buildRules :: Rules ()
buildRules = do
    sequence_ $ format <$> dhallPkgs
    sequence_ $ hpack <$> dhallPkgs
    phony "dhall-format" $ need $ (\x -> "dhall-format-" ++ x) <$> dhallPkgs
    phony "hpack-dhall" $ need $ (\x -> "hpack-dhall-" ++ x) <$> dhallPkgs
