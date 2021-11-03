module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Comp.hs"
    , "library/Flight/Pilot.hs"
    , "library/Flight/Path.hs"
    , "library/Flight/Path/Find.hs"
    , "library/Flight/Track/Curry.hs"
    , "library/Flight/Track/Distance.hs"
    , "library/Flight/Track/Point.hs"
    , "library/Flight/Path/Types.hs"
    , "library/Flight/Path/Tx.hs"
    , "library/Flight/Track/Place.hs"
    , "library/Flight/Track/Time.hs"
    , "-XDataKinds"
    , "-XDeriveAnyClass"
    , "-XDeriveGeneric"
    , "-XDerivingStrategies"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGeneralizedNewtypeDeriving"
    , "-XLambdaCase"
    , "-XMultiParamTypeClasses"
    , "-XMultiWayIf"
    , "-XNamedFieldPuns"
    , "-XOverloadedStrings"
    , "-XPackageImports"
    , "-XParallelListComp"
    , "-XPartialTypeSignatures"
    , "-XQuasiQuotes"
    , "-XRecordWildCards"
    , "-XStandaloneDeriving"
    , "-XScopedTypeVariables"
    , "-XTupleSections"
    , "-XTypeApplications"
    , "-XUndecidableInstances"

    , "-fplugin Data.UnitsOfMeasure.Plugin"
    ]

main :: IO ()
main = doctest arguments
