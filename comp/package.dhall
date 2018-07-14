    let defs = ./defaults.dhall 

in    defs
    ⫽ ./default-extensions.dhall 
    ⫽ { name =
          "flight-comp"
      , synopsis =
          "Hang gliding and paragliding competition scoring inputs."
      , description =
          "Hang gliding and paragliding competitors and tasks."
      , category =
          "Data"
      , github =
          "blockscope/flare-timing/comp"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
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
            , "lens"
            , "mtl"
            , "uom-plugin"
            , "vector"
            , "detour-via-sci"
            , "flight-units"
            , "flight-latlng"
            , "flight-zone"
            , "flight-route"
            , "flight-gap"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Flight.Comp"
              , "Flight.Track.Cross"
              , "Flight.Track.Tag"
              , "Flight.Track.Time"
              , "Flight.Track.Mask"
              , "Flight.Track.Land"
              , "Flight.Track.Point"
              , "Flight.Track.Speed"
              , "Flight.Track.Arrival"
              , "Flight.Track.Lead"
              , "Flight.Track.Distance"
              ]
          }
      , tests =
            ./default-tests.dhall 
          ⫽ { comp =
                { dependencies =
                    [ "tasty"
                    , "tasty-hunit"
                    , "tasty-quickcheck"
                    , "tasty-smallcheck"
                    , "smallcheck"
                    ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "TestCompMain.hs"
                , source-dirs =
                    [ "library", "test-suite-comp" ]
                }
            }
      }
