    let defs = ./defaults.dhall

in    defs
    ⫽ ./default-extensions.dhall
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
      , dependencies =
            defs.dependencies
          # [ "numbers"
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
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Task" }
      , tests =
            ./default-tests.dhall
          ⫽ { task =
                { dependencies =
                    [ "tasty"
                    , "tasty-hunit"
                    , "tasty-quickcheck"
                    , "tasty-smallcheck"
                    , "tasty-compare"
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
