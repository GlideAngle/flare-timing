    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flight-fsdb"
      , synopsis =
          "A parser for fsdb, the database XML format of FS."
      , description =
          "Hang gliding and paragliding competitors, tasks and results as XML."
      , category =
          "Data, Parsing"
      , github =
          "blockscope/flare-timing/fsdb"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
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
            , "flight-gap"
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
                    defs.dependencies # [ "doctest", "hxt-pickle-utils" ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "DocTest.hs"
                , source-dirs =
                    [ "library", "test-suite-doctest" ]
                , when =
                    { condition =
                        "flag(suppress-failing-tests)"
                    , buildable =
                        False
                    }
                }
            }
      }
