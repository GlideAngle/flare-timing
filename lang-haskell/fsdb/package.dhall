let mkHome = ./../home.dhall

in  let defs = ./../defaults.dhall

    in    defs
        ⫽ ./../default-extensions.dhall
        ⫽ { name = "flight-fsdb"
          , homepage = mkHome "lang-haskell/fsdb#readme"
          , synopsis = "A parser for fsdb, the database XML format of FS."
          , description =
              "Hang gliding and paragliding competitors, tasks and results as XML."
          , category = "Data, Parsing"
          , ghc-options = [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
          , dependencies =
                defs.dependencies
              # [ "aeson"
                , "containers"
                , "facts"
                , "hxt"
                , "hxt-xpath"
                , "megaparsec ^>= 7.0.4"
                , "newtype"
                , "path"
                , "scientific"
                , "split"
                , "statistics"
                , "time"
                , "vector"
                , "uom-plugin"
                , "detour-via-sci"
                , "flight-comp"
                , "flight-earth"
                , "flight-gap-allot"
                , "flight-gap-effort"
                , "flight-gap-stop"
                , "flight-gap-lead"
                , "flight-gap-math"
                , "flight-gap-valid"
                , "flight-gap-weight"
                , "flight-latlng"
                , "flight-units"
                , "flight-zone"
                ]
          , library =
            { source-dirs = "library", exposed-modules = "Flight.Fsdb" }
          , tests =
                ./../default-tests.dhall
              ⫽ { doctest =
                  { dependencies =
                        defs.dependencies
                      # [ "doctest", "hxt-pickle-utils", "flight-comp" ]
                  , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                  , main = "DocTest.hs"
                  , source-dirs = [ "library", "test-suite-doctest" ]
                  }
                }
          }
