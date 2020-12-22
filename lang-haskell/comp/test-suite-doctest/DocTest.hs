module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Path/Types.hs"
    , "library/Flight/Path/Tx.hs"
    , "library/Flight/Track/Place.hs"
    , "-XDataKinds"
    , "-XDerivingStrategies"
    , "-XFlexibleContexts"
    , "-XGeneralizedNewtypeDeriving"
    , "-XPackageImports"
    , "-XTypeApplications"

    , "-package=QuickCheck"
    ]

main :: IO ()
main = doctest arguments
