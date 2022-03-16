let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-track"
          , homepage = mkHome "lang-haskell/track#readme"
          , synopsis = "Hang gliding and paragliding competition track logs."
          , description =
              "Reading track logs for each pilot in each task of a competition."
          , category = "Data"
          , extra-source-files = defs.extra-source-files # [ "**/*.igc" ]
          , dependencies =
                defs.dependencies
              # [ "split"
                , "path"
                , "containers"
                , "mtl"
                , "deepseq"
                , "directory"
                , "filepath"
                , "time"
                , "bytestring"
                , "utf8-string"
                , "flight-clip"
                , "flight-comp"
                , "flight-kml"
                , "flight-igc"
                ]
          , library =
            { source-dirs = "library", exposed-modules = "Flight.TrackLog" }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                  { dependencies =
                      defs.dependencies # [ "doctest", "QuickCheck" ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "DocTest.hs"
                  , source-dirs = [ "library", "test-suite-doctest" ]
                  }
                }
          }
