    let defs = ./defaults.dhall 

in    defs
    ⫽ { name =
          "flight-scribe"
      , synopsis =
          "Hang gliding and paragliding competition scoring files."
      , description =
          "Reading and writing competition scoring files."
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/scribe"
      , dependencies =
            defs.dependencies
          # [ "split"
            , "path"
            , "aeson"
            , "scientific"
            , "containers"
            , "unordered-containers"
            , "time"
            , "cassava"
            , "bytestring"
            , "directory"
            , "filepath"
            , "filemanip"
            , "mtl"
            , "yaml"
            , "vector"
            , "detour-via-sci"
            , "flight-latlng"
            , "flight-zone"
            , "flight-route"
            , "flight-gap"
            , "flight-comp"
            ]
      , library =
          { source-dirs = "library", exposed-modules = "Flight.Scribe" }
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
