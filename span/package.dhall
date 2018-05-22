./defaults.dhall //
{ name =
    "flight-span"
, synopsis =
    "How to measure a distance that spans two points."
, description =
    "How to measure a distance that spans tow points."
, category =
    "Data"
, github =
    "blockscope/flare-timing/span"
, library =
    { dependencies =
        [ "base >=4.5 && <5", "cmdargs" ]
    , source-dirs =
        "library"
    , exposed-modules =
        "Flight.Span.Math"
    }
, tests =
    { hlint =
        { dependencies =
            [ "base", "hlint", "cmdargs" ]
        , ghc-options =
            [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
        , main =
            "HLint.hs"
        , source-dirs =
            [ "library", "test-suite-hlint" ]
        }
    }
}
