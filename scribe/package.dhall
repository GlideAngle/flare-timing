  ./defaults.dhall 
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
  , library =
      { dependencies =
          [ "base >=4.5 && <5"
          , "split"
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
      , source-dirs =
          "library"
      , exposed-modules =
          "Flight.Scribe"
      }
  , tests =
      { hlint =
          { dependencies =
              [ "base"
              , "hlint"
              , "split"
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
          , ghc-options =
              [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
          , main =
              "HLint.hs"
          , source-dirs =
              [ "library", "test-suite-hlint" ]
          }
      }
  }
