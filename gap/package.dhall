  ./defaults.dhall 
⫽ ./default-extensions.dhall 
⫽ { name =
      "flight-gap"
  , synopsis =
      "GAP Scoring."
  , description =
      "GAP scoring for hang gliding and paragliding competitons."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/gap"
  , ghc-options =
      [ "-Wall"
      , "-fplugin Data.UnitsOfMeasure.Plugin"
      , "-fno-warn-partial-type-signatures"
      ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "containers"
          , "vector"
          , "statistics"
          , "aeson"
          , "newtype"
          , "scientific"
          , "uom-plugin"
          , "template-haskell"
          , "detour-via-sci"
          , "detour-via-uom"
          , "siggy-chardust"
          , "flight-units"
          , "liquid-fixpoint"
          , "liquidhaskell"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          "Flight.Score"
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "containers"
              , "vector"
              , "statistics"
              , "aeson"
              , "newtype"
              , "scientific"
              , "uom-plugin"
              , "detour-via-sci"
              , "detour-via-uom"
              , "siggy-chardust"
              , "flight-units"
              ]
          , ghc-options =
              [ "-rtsopts"
              , "-threaded"
              , "-with-rtsopts=-N"
              , "-fplugin Data.UnitsOfMeasure.Plugin"
              ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      , score =
          { dependencies =
              [ "base"
              , "containers"
              , "vector"
              , "statistics"
              , "aeson"
              , "newtype"
              , "scientific"
              , "uom-plugin"
              , "detour-via-sci"
              , "detour-via-uom"
              , "siggy-chardust"
              , "flight-units"
              , "tasty"
              , "tasty-hunit"
              , "tasty-quickcheck"
              , "tasty-smallcheck"
              , "smallcheck"
              ]
          , ghc-options =
              [ "-rtsopts"
              , "-threaded"
              , "-with-rtsopts=-N"
              , "-fplugin Data.UnitsOfMeasure.Plugin"
              ]
          , main =
              "Score.hs"
          , source-dirs =
              [ "library", "test-suite-score" ]
          }
      }
  }
