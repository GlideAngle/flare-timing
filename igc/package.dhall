./defaults.dhall //
{ name =
    "flight-igc"
, synopsis =
    "A parser for IGC files."
, description =
    "IGC is a waypoint file format from the International Gliding Commission of FAI. This haskell library can parse B records from these files."
, category =
    "Data, Parsing"
, github =
    "blockscope/flare-timing/igc"
, library =
    { dependencies =
        [ "base >=4.5 && <5", "parsec" ]
    , source-dirs =
        "library"
    , exposed-modules =
        "Flight.Igc"
    }
, tests =
    { hlint =
        { dependencies =
            [ "base", "hlint", "flight-igc " ]
        , ghc-options =
            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main =
            "HLint.hs"
        , source-dirs =
            "test-suite"
        }
    }
}
