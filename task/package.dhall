  ./defaults.dhall 
⫽ { name =
      "flight-task"
  , synopsis =
      "Tasks to fly."
  , description =
      "Tasks for hang gliding and paragliding competitons."
  , category =
      "Flight"
  , github =
      "blockscope/flare-timing/task"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "numbers"
          , "fgl"
          , "uom-plugin"
          , "bifunctors"
          , "aeson"
          , "scientific"
          , "mtl"
          , "detour-via-sci"
          , "siggy-chardust"
          , "flight-units"
          , "flight-latlng"
          , "flight-zone"
          , "flight-earth"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          "Flight.Task"
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "numbers"
              , "fgl"
              , "uom-plugin"
              , "bifunctors"
              , "aeson"
              , "scientific"
              , "mtl"
              , "detour-via-sci"
              , "siggy-chardust"
              , "tasty-compare"
              , "flight-units"
              , "flight-latlng"
              , "flight-zone"
              , "flight-earth"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      , task =
          { dependencies =
              [ "base"
              , "numbers"
              , "fgl"
              , "bifunctors"
              , "uom-plugin"
              , "aeson"
              , "scientific"
              , "mtl"
              , "hcoord"
              , "siggy-chardust"
              , "tasty-compare"
              , "flight-units"
              , "flight-latlng"
              , "flight-zone"
              , "flight-earth"
              , "tasty"
              , "tasty-hunit"
              , "tasty-quickcheck"
              , "tasty-smallcheck"
              , "smallcheck"
              ]
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "Task.hs"
          , source-dirs =
              [ "library", "test-suite-task" ]
          }
      }
  }
