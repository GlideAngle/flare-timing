    let defs = ./defaults.dhall 

in    defs
    ⫽ { name =
          "flight-span"
      , synopsis =
          "How to measure a distance that spans two points."
      , description =
          "How to measure a distance that spans tow points."
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/span"
      , dependencies =
          defs.dependencies # [ "cmdargs" ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Span.Math" }
      , tests =
          { hlint =
              { dependencies =
                  [ "hlint" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  [ "library", "test-suite-hlint" ]
              }
          }
      }
