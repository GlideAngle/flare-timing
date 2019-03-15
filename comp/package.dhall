    let defs = ./../defaults.dhall

in    defs
    ⫽ ./../default-extensions.dhall
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
          # [ "aeson"
            , "bytestring"
            , "cassava"
            , "containers"
            , "directory"
            , "filepath"
            , "filemanip"
            , "lens"
            , "mtl"
            , "path"
            , "scientific"
            , "split"
            , "text"
            , "time"
            , "unordered-containers"
            , "uom-plugin"
            , "vector"
            , "siggy-chardust"
            , "detour-via-sci"
            , "detour-via-uom"
            , "flight-clip"
            , "flight-earth"
            , "flight-latlng"
            , "flight-gap"
            , "flight-route"
            , "flight-zone"
            , "flight-units"
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
              , "Flight.Track.Place"
              , "Flight.Track.Point"
              , "Flight.Track.Speed"
              , "Flight.Track.Arrival"
              , "Flight.Track.Lead"
              , "Flight.Track.Distance"
              ]
          }
      , tests =
            ./../default-tests.dhall
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
            , doctest =
                { dependencies =
                    [ "doctest" ]
                , ghc-options =
                    [ "-rtsopts", "-threaded", "-with-rtsopts=-N" ]
                , main =
                    "DocTest.hs"
                , source-dirs =
                    [ "library", "test-suite-doctest" ]
                }
            }
      }
