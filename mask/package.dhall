  ./defaults.dhall 
⫽ { name =
      "flight-mask"
  , synopsis =
      "Track logs masked by competition task zones."
  , description =
      "Masking tracks with zones, work out; did the pilot launch, did they make goal and how long did that take? What was distance to goal?"
  , category =
      "Data"
  , github =
      "blockscope/flare-timing/mask"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , default-extensions =
      [ "ScopedTypeVariables"
      , "DataKinds"
      , "FlexibleContexts"
      , "FlexibleInstances"
      , "MultiParamTypeClasses"
      , "TypeOperators"
      , "TypeFamilies"
      , "UndecidableInstances"
      , "QuasiQuotes"
      , "DisambiguateRecordFields"
      , "NamedFieldPuns"
      , "LambdaCase"
      , "PartialTypeSignatures"
      , "RecordWildCards"
      ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "split"
          , "path"
          , "containers"
          , "mtl"
          , "directory"
          , "filepath"
          , "time"
          , "lens"
          , "bytestring"
          , "yaml"
          , "uom-plugin"
          , "fgl"
          , "cmdargs"
          , "numbers"
          , "aeson-via-sci"
          , "flight-span"
          , "flight-units"
          , "flight-comp"
          , "flight-kml"
          , "flight-latlng"
          , "flight-zone"
          , "flight-route"
          , "flight-track"
          , "flight-earth"
          , "flight-task"
          , "flight-gap"
          , "flight-scribe"
          , "siggy-chardust"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Flight.Mask"
          , "Flight.Comp.Distance"
          , "Flight.Span.Double"
          , "Flight.Span.Rational"
          ]
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "containers"
              , "split"
              , "mtl"
              , "directory"
              , "filepath"
              , "time"
              , "lens"
              , "bytestring"
              , "yaml"
              , "fgl"
              , "uom-plugin"
              , "cmdargs"
              , "numbers"
              , "aeson-via-sci"
              , "flight-span"
              , "flight-units"
              , "flight-comp"
              , "flight-kml"
              , "flight-latlng"
              , "flight-zone"
              , "flight-route"
              , "flight-track"
              , "flight-task"
              , "flight-earth"
              , "flight-gap"
              , "flight-scribe"
              , "siggy-chardust"
              ]
          , ghc-options =
              [ "-rtsopts"
              , "-threaded"
              , "-with-rtsopts=-N"
              , "-fplugin Data.UnitsOfMeasure.Plugin"
              ]
          , default-extensions =
              [ "ScopedTypeVariables"
              , "DataKinds"
              , "FlexibleContexts"
              , "FlexibleInstances"
              , "MultiParamTypeClasses"
              , "TypeOperators"
              , "TypeFamilies"
              , "UndecidableInstances"
              , "QuasiQuotes"
              , "DisambiguateRecordFields"
              , "NamedFieldPuns"
              , "LambdaCase"
              , "PartialTypeSignatures"
              , "RecordWildCards"
              ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      }
  }
