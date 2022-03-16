let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in  let testdeps =
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
              , "QuickCheck"
              , "quickcheck-instances"
              ]

        in  let testopts =
                  [ "-rtsopts"
                  , "-threaded"
                  , "-with-rtsopts=-N"
                  , "-fplugin Data.UnitsOfMeasure.Plugin"
                  ]

            in    defs
                ⫽ ./../default-extensions.dhall
                ⫽ { flags.suppress-failing-tests
                    =
                    { manual = False, default = True }
                  , name = "flight-gap-weight"
                  , homepage = mkHome "lang-haskell/gap-weight#readme"
                  , synopsis = "GAP Scoring, Weights"
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons, the weighting parts."
                  , category = "Flight"
                  , ghc-options =
                    [ "-Wall"
                    , "-fplugin Data.UnitsOfMeasure.Plugin"
                    , "-fno-warn-partial-type-signatures"
                    ]
                  , dependencies =
                        defs.dependencies
                      # [ "aeson"
                        , "cassava"
                        , "containers"
                        , "facts"
                        , "newtype"
                        , "numbers"
                        , "QuickCheck"
                        , "scientific"
                        , "template-haskell"
                        , "text"
                        , "uom-plugin"
                        , "detour-via-sci"
                        , "detour-via-uom"
                        , "siggy-chardust"
                        , "flight-units"
                        , "flight-gap-allot"
                        ]
                  , library =
                    { source-dirs = "library"
                    , exposed-modules = "Flight.Score"
                    }
                  , tests =
                        ./../default-tests.dhall
                      ⫽ { weight =
                          { dependencies =
                              testdeps # [ "facts", "flight-gap-weight" ]
                          , ghc-options = testopts
                          , main = "WeightTestMain.hs"
                          , source-dirs = "test-suite-weight"
                          }
                        }
                  }
