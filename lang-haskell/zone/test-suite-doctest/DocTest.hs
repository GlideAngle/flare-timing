module Main (main) where

import Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "-isrc"
    , "library/Flight/Zone/SpeedSection.hs"
    , "library/Flight/Zone/Radius.hs"
    , "library/Flight/Zone/Raw/Zone.hs"
    , "-fplugin=Data.UnitsOfMeasure.Plugin"
    , "-fno-warn-partial-type-signatures"
    , "-XDataKinds"
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
    ]

main :: IO ()
main = doctest arguments
