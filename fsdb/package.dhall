./defaults.dhall //
{ name =
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
, library =
    { dependencies =
        [ "base >=4.5 && <5"
        , "split"
        , "megaparsec"
        , "hxt"
        , "path"
        , "hxt-xpath"
        , "aeson"
        , "scientific"
        , "containers"
        , "split"
        , "time"
        , "uom-plugin"
        , "aeson-via-sci"
        , "flight-latlng"
        , "flight-units"
        , "flight-zone"
        , "flight-comp"
        , "flight-gap"
        ]
    , source-dirs =
        "library"
    , exposed-modules =
        "Flight.Fsdb"
    }
, tests =
    { hlint =
        { dependencies =
            [ "base"
            , "megaparsec"
            , "hxt"
            , "path"
            , "hxt-xpath"
            , "hlint"
            , "aeson"
            , "scientific"
            , "containers"
            , "split"
            , "time"
            , "aeson-via-sci"
            , "flight-latlng"
            , "flight-units"
            , "flight-zone"
            , "flight-comp"
            ]
        , ghc-options =
            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main =
            "HLint.hs"
        , source-dirs =
            [ "library", "test-suite-hlint" ]
        }
    , parse =
        { dependencies =
            [ "base"
            , "parsec"
            , "hxt"
            , "path"
            , "hxt-xpath"
            , "raw-strings-qq"
            , "split"
            , "time"
            , "tasty"
            , "tasty-hunit"
            , "tasty-quickcheck"
            , "tasty-smallcheck"
            , "smallcheck"
            , "aeson"
            , "scientific"
            , "containers"
            , "flight-comp"
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
