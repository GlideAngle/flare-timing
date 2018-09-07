    let defs = ./defaults.dhall

in    defs
    ⫽ ./default-extensions.dhall
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
          # [ "split"
            , "megaparsec"
            , "hxt"
            , "path"
            , "hxt-xpath"
            , "aeson"
            , "scientific"
            , "containers"
            , "split"
            , "time"
            , "newtype"
            , "uom-plugin"
            , "detour-via-sci"
            , "flight-latlng"
            , "flight-units"
            , "flight-zone"
            , "flight-comp"
            , "flight-gap"
            ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Fsdb" }
      , tests =
            ./default-tests.dhall
          ⫽ { parse =
                { dependencies =
                    [ "tasty"
                    , "tasty-hunit"
                    , "tasty-quickcheck"
                    , "tasty-smallcheck"
                    , "smallcheck"
                    ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "Parse.hs"
                , source-dirs =
                    [ "library", "test-suite-parse" ]
                }
            }
      }
