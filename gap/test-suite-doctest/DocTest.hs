module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Gap/Ratio/Arrival.hs"
    , "library/Flight/Gap/Ratio/Speed.hs"
    , "library/Flight/Gap/Place/Arrival.hs"
    , "library/Flight/Gap/Time/Best.hs"
    , "library/Flight/Gap/Time/Pilot.hs"
    , "library/Flight/Gap/Allot.hs"
    , "library/Flight/Gap/Pilots.hs"
    , "-XDataKinds"
    , "-XDeriveAnyClass"
    , "-XDeriveGeneric"
    , "-XDerivingStrategies"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XMultiParamTypeClasses"
    , "-XPackageImports"
    , "-XTemplateHaskell"
    , "-XQuasiQuotes"
    , "-fplugin Data.UnitsOfMeasure.Plugin"
    ]

main :: IO ()
main = doctest arguments
