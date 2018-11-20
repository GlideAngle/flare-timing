{ default-extensions =
    [ "DataKinds"
    , "DeriveFunctor"
    , "DeriveGeneric"
    , "DeriveAnyClass"
    -- WARNING: DerivingStrategies is an extension not known to the compiler in
    -- use with reflex-platform.
    --  Configuring flight-igc-1.0.0...
    --   Warning: Unknown extensions: DerivingStrategies
    --   Dependency base >=4.9.1.0 && <5: using base-4.9.1.0
    --   Dependency bytestring -any: using bytestring-0.10.8.1
    --   Dependency parsec -any: using parsec-3.1.11
    --   Dependency utf8-string -any: using utf8-string-1.0.1.1
    --   Setup: The package flight-igc-1.0.0 requires the following language extensions
    --   which are not supported by ghc-8.0.2: DerivingStrategies
    --
    -- , "DerivingStrategies"
    , "DisambiguateRecordFields"
    , "FlexibleContexts"
    , "FlexibleInstances"
    , "GeneralizedNewtypeDeriving"
    , "GADTs"
    , "LambdaCase"
    , "MonoLocalBinds"
    , "MultiParamTypeClasses"
    , "MultiWayIf"
    , "NamedFieldPuns"
    , "OverloadedStrings"
    , "PackageImports"
    , "ParallelListComp"
    , "PartialTypeSignatures"
    , "PatternSynonyms"
    , "QuasiQuotes"
    , "RankNTypes"
    , "RecordWildCards"
    , "ScopedTypeVariables"
    , "StandaloneDeriving"
    , "TemplateHaskell"
    , "TypeApplications"
    , "TypeFamilies"
    , "TypeOperators"
    , "TypeSynonymInstances"
    , "TupleSections"
    , "UndecidableInstances"
    ]
}
