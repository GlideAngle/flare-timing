  ./defaults.dhall 
⫽ { name =
      "flight-lookup"
  , synopsis =
      "Hang gliding and paragliding competition data access."
  , description =
      "Lookup items for a task, for a pilot, etc."
  , category =
      "Data"
  , github =
      "blockscope/flare-timing/lookup"
  , ghc-options =
      [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
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
          , "lens"
          , "mtl"
          , "uom-plugin"
          , "detour-via-sci"
          , "flight-latlng"
          , "flight-zone"
          , "flight-route"
          , "flight-gap"
          , "flight-comp"
          , "flight-kml"
          , "flight-mask"
          ]
      , source-dirs =
          "library"
      , exposed-modules =
          [ "Flight.Lookup.Route"
          , "Flight.Lookup.Cross"
          , "Flight.Lookup.Tag"
          , "Flight.Lookup"
          ]
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
              , "lens"
              , "mtl"
              , "uom-plugin"
              , "detour-via-sci"
              , "flight-latlng"
              , "flight-zone"
              , "flight-route"
              , "flight-gap"
              , "flight-comp"
              , "flight-kml"
              , "flight-mask"
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
