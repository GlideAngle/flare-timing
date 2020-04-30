module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-XDataKinds"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XDeriveAnyClass"
    , "-XDerivingStrategies"
    , "-XDisambiguateRecordFields"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGeneralizedNewtypeDeriving"
    , "-XGADTs"
    , "-XLambdaCase"
    , "-XMultiParamTypeClasses"
    , "-XMultiWayIf"
    , "-XNamedFieldPuns"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XParallelListComp"
    , "-XPartialTypeSignatures"
    , "-XPatternSynonyms"
    , "-XQuasiQuotes"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTemplateHaskell"
    , "-XTypeApplications"
    , "-XTypeFamilies"
    , "-XTypeOperators"
    , "-XTypeSynonymInstances"
    , "-XTupleSections"
    , "-XUndecidableInstances"

    , "-XAllowAmbiguousTypes"
    , "-XInstanceSigs"
    , "-XUndecidableSuperClasses"

    , "-isrc"
    , "-fplugin=Data.UnitsOfMeasure.Plugin"
    , "-fno-warn-partial-type-signatures"

    , "-package=facts"

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
    , "library/Flight/Gap/Points/Task.hs"
    , "library/Flight/Gap/Penalty.hs"
    ]

main :: IO ()
main = doctest arguments
