let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-igc"
          , homepage = mkHome "lang-haskell/igc#readme"
          , version = "2.0.0"
          , synopsis = "A parser for IGC files."
          , description =
              "IGC is a waypoint file format from the International Gliding Commission of FAI. This haskell library can parse B records from these files."
          , category = "data, parsing"
          , extra-source-files = defs.extra-source-files # [ "**/*.igc" ]
          , dependencies =
                defs.dependencies
              # [ "bytestring"
                , "flight-clip"
                , "deepseq"
                , "attoparsec"
                , "time"
                , "utf8-string"
                , "QuickCheck"
                ]
          , library =
            { source-dirs = "library", exposed-modules = "Flight.Igc" }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                  { dependencies =
                      defs.dependencies # [ "doctest", "QuickCheck" ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "DocTest.hs"
                  , source-dirs = [ "library", "test-suite-doctest" ]
                  , when =
                    { condition = "flag(suppress-failing-tests)"
                    , buildable = True
                    }
                  }
                }
          }
