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
                  , name = "flight-gap"
                  , homepage = mkHome "lang-haskell/gap#readme"
                  , synopsis = "GAP Scoring."
                  , description =
                      "GAP scoring for hang gliding and paragliding competitons."
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
                        , "flight-gap-effort"
                        , "flight-gap-lead"
                        , "flight-gap-math"
                        , "flight-gap-stop"
                        , "flight-gap-valid"
                        , "flight-gap-weight"
                        ]
                  , library =
                    { source-dirs = "library"
                    , exposed-modules = "Flight.Score"
                    }
                  }
