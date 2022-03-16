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
                  , name = "flight-gap-valid"
                  , homepage = mkHome "lang-haskell/gap-valid#readme"
                  , synopsis = "GAP Scoring Validities"
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons, the validity parts."
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
                      ⫽ { valid =
                          { dependencies = testdeps # [ "flight-gap-valid" ]
                          , ghc-options = testopts
                          , main = "ValidityTestMain.hs"
                          , source-dirs = "test-suite-valid"
                          }
                        }
                  }
