    let defs = ./defaults.dhall 

in    defs
    ⫽ { name =
          "flight-igc"
      , synopsis =
          "A parser for IGC files."
      , description =
          "IGC is a waypoint file format from the International Gliding Commission of FAI. This haskell library can parse B records from these files."
      , category =
          "data, parsing"
      , github =
          "blockscope/flare-timing/igc"
      , dependencies =
          defs.dependencies # [ "parsec", "bytestring", "utf8-string" ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Igc" }
      , tests =
          { hlint =
              { dependencies =
                  [ "hlint" ]
              , ghc-options =
                  [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
              , main =
                  "HLint.hs"
              , source-dirs =
                  "test-suite"
              }
          }
      }
