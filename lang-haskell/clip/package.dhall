    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
    ⫽ { name =
          "flight-clip"
      , version =
          "1.1.0"
      , synopsis =
          "Clipping a pilot's tracklogs."
      , description =
          "Provides types and typeclasses for clipping a tracklog."
      , category =
          "Data, Parsing, Flight"
      , github =
          "blockscope/flare-timing/clip"
      , homepage =
          "https://github.com/blockscope/flare-timing/tree/master/clip#readme"
      , dependencies =
          defs.dependencies # [ "split", "time" ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Clip", "Flight.Track.Range" ]
          }
      , tests =
            ./../default-tests.dhall
          ⫽ { doctest =
                { dependencies =
                    [ "doctest", "QuickCheck" ]
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
