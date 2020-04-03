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

    , "library/Flight/Span/Sliver.hs"
    , "library/Flight/Span/Double.hs"
    , "library/Flight/Mask/Group.hs"
    , "library/Flight/Mask/Internal/Cross.hs"
    ]

main :: IO ()
main = doctest arguments
