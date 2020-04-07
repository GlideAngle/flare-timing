module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Gap/Distance/Chunk.hs"
    , "library/Flight/Gap/Distance/Pilot.hs"
    , "library/Flight/Gap/Distance/Relative.hs"
    , "library/Flight/Gap/Distance/Stop.hs"
    , "library/Flight/Gap/Fraction/Arrival.hs"
    , "library/Flight/Gap/Fraction/Speed.hs"
    , "library/Flight/Gap/Fraction/Difficulty.hs"
    , "library/Flight/Gap/Place/Arrival.hs"
    , "library/Flight/Gap/Time/Arrival.hs"
    , "library/Flight/Gap/Time/Best.hs"
    , "library/Flight/Gap/Time/Pilot.hs"
    , "library/Flight/Gap/Allot.hs"
    , "library/Flight/Gap/Pilots.hs"
    , "library/Flight/Gap/Equation.hs"
    , "-XDataKinds"
    , "-XDeriveAnyClass"
    , "-XDeriveGeneric"
    , "-XDerivingStrategies"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XMultiParamTypeClasses"
    , "-XNamedFieldPuns"
    , "-XPackageImports"
    , "-XParallelListComp"
    , "-XPartialTypeSignatures"
    , "-XTemplateHaskell"
    , "-XQuasiQuotes"
    , "-XUndecidableInstances"
    , "-fplugin Data.UnitsOfMeasure.Plugin"
    , "-fno-warn-partial-type-signatures"
    ]

main :: IO ()
main = doctest arguments
