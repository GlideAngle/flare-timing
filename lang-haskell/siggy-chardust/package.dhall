let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in    defs
        ⫽ { name = "siggy-chardust"
          , homepage = mkHome "lang-haskell/siggy-chardust#readme"
          , version = "1.0.0"
          , synopsis =
              "Rounding rationals to significant digits and decimal places."
          , description =
              "The round function from the prelude returns an integer. The standard librarys of C and C++ have round functions that return floating point numbers. Rounding in this library takes and returns rationals and can round to a number of significant digits or a number of decimal places."
          , category = "Data, Math, Numeric"
          , library =
            { source-dirs = "library", exposed-modules = "Data.Ratio.Rounding" }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                  { dependencies = defs.dependencies # [ "doctest" ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "DocTest.hs"
                  , source-dirs = [ "library", "test-suite-doctest" ]
                  }
                , digits =
                  { dependencies =
                    [ "tasty"
                    , "tasty-hunit"
                    , "tasty-quickcheck"
                    , "tasty-smallcheck"
                    , "smallcheck"
                    ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "Rounding.hs"
                  , source-dirs = [ "library", "test-suite-digits" ]
                  }
                }
          }
