    let defs = ./defaults.dhall 

in    defs
    ⫽ { name =
          "flight-track"
      , synopsis =
          "Hang gliding and paragliding competition track logs."
      , description =
          "Reading track logs for each pilot in each task of a competition."
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/track"
      , dependencies =
            defs.dependencies
          # [ "split"
            , "path"
            , "containers"
            , "mtl"
            , "directory"
            , "filepath"
            , "time"
            , "bytestring"
            , "utf8-string"
            , "flight-comp"
            , "flight-kml"
            , "flight-igc"
            ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.TrackLog" }
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
