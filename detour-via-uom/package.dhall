    let defs = ./../defaults.dhall

in    defs
    ⫽ { name =
          "detour-via-uom"
      , version =
          "1.0.1"
      , synopsis =
          "JSON and CSV encoding for quantities."
      , description =
          "Lossy JSON and CSV encoding and decoding for newtype quantities via scientific with fixed decimal places and with units."
      , category =
          "Data, Math, Numeric, JSON, CSV, Physics"
      , github =
          "blockscope/flare-timing/detour-via-uom"
      , homepage =
          "https://github.com/blockscope/flare-timing/tree/master/detour-via-uom#readme"
      , ghc-options =
          [ "-Wall", "-fplugin Data.UnitsOfMeasure.Plugin" ]
      , dependencies =
            defs.dependencies
          # [ "newtype"
            , "scientific"
            , "aeson"
            , "cassava"
            , "uom-plugin"
            , "detour-via-sci"
            ]
      , library =
          { source-dirs =
              "library"
          , exposed-modules =
              [ "Data.Via.UnitsOfMeasure" ]
          }
      , tests =
            ./../default-tests.dhall
          ⫽ { doctest =
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
