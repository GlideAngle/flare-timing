./defaults.dhall //
{ name =
    "flight-track"
, synopsis =
    "Hang gliding and paragliding competition track logs."
, description =
    "Reading track logs for each pilot in each task of a competition."
, category =
    "Data"
, github =
    "blockscope/flare-timing/track"
, library =
    { dependencies =
        [ "base >=4.5 && <5"
        , "split"
        , "path"
        , "containers"
        , "mtl"
        , "directory"
        , "filepath"
        , "time"
        , "flight-comp"
        , "flight-kml"
        , "flight-igc"
        ]
    , source-dirs =
        "library"
    , exposed-modules =
        "Flight.TrackLog"
    }
, tests =
    { hlint =
        { dependencies =
            [ "base"
            , "hlint"
            , "containers"
            , "split"
            , "mtl"
            , "directory"
            , "filepath"
            , "time"
            , "flight-comp"
            , "flight-kml"
            ]
        , ghc-options =
            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main =
            "HLint.hs"
        , source-dirs =
            [ "library", "test-suite-hlint" ]
        }
    }
}
