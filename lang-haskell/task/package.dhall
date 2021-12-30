let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in  let exts = ./../default-extensions.dhall

        in    defs
            ⫽ { name = "flight-task"
              , homepage = mkHome "lang-haskell/task#readme"
              , synopsis = "Tasks to fly."
              , description =
                  "Tasks for hang gliding and paragliding competitons."
              , category = "Flight"
              , ghc-options = [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
              , default-extensions =
                    exts.default-extensions
                  # [ "AllowAmbiguousTypes"
                    , "InstanceSigs"
                    , "UndecidableSuperClasses"
                    ]
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
                    , "flight-earth"
                    , "flight-latlng"
                    , "flight-units"
                    , "flight-zone"
                    ]
              , library =
                { source-dirs = "library"
                , exposed-modules =
                  [ "Flight.ShortestPath"
                  , "Flight.ShortestPath.Double"
                  , "Flight.ShortestPath.Rational"
                  , "Flight.Task"
                  ]
                }
              , tests =
                    ./../default-tests.dhall
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
                      , main = "Task.hs"
                      , source-dirs = [ "library", "test-suite-task" ]
                      , when =
                        { condition = "flag(suppress-failing-tests)"
                        , buildable = False
                        }
                      }
                    }
              }
